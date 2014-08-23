{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.Topic where

import Safe (headMay)
import Control.Monad (when, unless, (>=>), void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow, throwM)
import Database.Groundhog ((==.), (&&.), (=.))
import qualified Database.Groundhog as GH
import Control.Applicative (pure, (<$>))
import Data.Maybe (isJust)
import Data.Either (isLeft)
import Data.Bool (bool)

import Model.IDGen (genRandom)
import Model.ID (ServerId, UserId, TopicId, MessageId)
import qualified Model.User as Ur
import qualified Model.Topic as Tp
import qualified Model.TopicMember as Tm
import Model.StorableJson (StorableJson)
import Model.Message (Message)
import Model.Message as Msg

import Operations.Base

getTopic :: CatchDbConn m cm conn => Tp.TopicRef -> m (Either OpError Tp.Topic)
getTopic = runOp . opGetTopic

setTopicMode :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Tp.TopicModeUpdate -> m (Either OpError Message)
setTopicMode caller tr tmu = runOp $ do
    topic <- testTopicPerm caller tr (const Tm.mmSetMode) SetTopicMode
    let newMode = Tp.resolveTopicModeUpdate (Tp.topicMode topic) tmu
    GH.update [Tp.TopicModeField =. newMode] $ Tp.TopicCoord ==. tr
    opCreateMessage caller tr $ MsgTopicModeChanged newMode

listMembers :: (CatchDbConn m cm conn, Functor m) => Ur.UserRef -> Tp.TopicRef -> m (Either OpError [Tm.MemberPartial])
listMembers callerRef tr = runOp $ do
    testTopicPerm_ callerRef tr (const Tm.mmRead) ReadTopic
    res <- GH.project (Tm.MemberUserField, Tm.MemberModeField) (Tm.MemberTopicField ==. tr)
    return $ uncurry Tm.MemberPartial <$> res

getMember :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> m (Either OpError Tm.MemberMode)
getMember callerRef tr ur = runOp $ do
    testTopicPerm_ callerRef tr (const Tm.mmRead) ReadTopic
    res <- GH.getBy (Tm.TargetMemberKey tr ur) 
    getOrThrow (MemberNotFound tr ur) (Tm.memberMode <$> res)

opGetTopic :: (GH.PersistBackend m, MonadThrow m) => Tp.TopicRef -> m Tp.Topic
opGetTopic tr = GH.getBy tr >>= getOrThrow (TopicNotFound tr)

--todo figure out best order of arguments
setMemberMode :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> Tm.MemberModeUpdate -> m (Either OpError Message)
setMemberMode callerRef tr targetUser mmu = runOp $ do
    testTopicPerm_ callerRef tr (const Tm.mmSetMember) SetMemberMode
    targetMember <- GH.getBy (Tm.TargetMemberKey tr targetUser) >>= getOrThrow (MemberNotFound tr targetUser)
    let newMode = Tm.resolveMemberModeUpdate (Tm.memberMode targetMember) mmu
    GH.update [Tm.MemberModeField =. newMode] $ (Tm.MemberTopicField ==. tr) &&. (Tm.MemberUserField ==. targetUser)
    opCreateMessage callerRef tr $ Msg.MsgMemberModeChanged targetUser newMode

testTopicPerm :: (MonadThrow m, Functor m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m Tp.Topic
testTopicPerm caller tr modeP op = do
    topic <- opGetTopic tr
    em <- getMemberEffectiveMode caller topic
    unless (modeP (Tp.topicMode topic) em) $ throwM (OperationDenied op)
    return topic

getMemberEffectiveMode :: (Functor m, GH.PersistBackend m) => Ur.UserRef -> Tp.Topic -> m Tm.MemberMode
getMemberEffectiveMode ur topic
    | ur `Tp.isCreator` topic = return Tm.modeCreator
    | otherwise =  extract <$> query 
        where query = GH.getBy $ Tm.TargetMemberKey (GH.extractUnique topic) ur
              extract = maybe (Tm.nonMemberMode (Tp.topicMode topic)) Tm.memberMode

testTopicPerm_ :: (MonadThrow m, Functor m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m ()
testTopicPerm_ caller tr mode op = void $ testTopicPerm caller tr mode op

createTopic :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicCreate -> m (Either OpError Tp.Topic)
createTopic ur tc = runOp $ do 
    tid <- maybe genRandom return $ Tp.createId tc
    let tr = Tp.fromUserRef tid ur
        newTopic = Tp.initializeTopic ur tid tc
        givenId = isJust $ Tp.createId tc
    --create a topic and insert the creator
    tcRes <- GH.insertByAll newTopic
    when (isLeft tcRes) $ throwM $ checkedFailure givenId ur tr
    GH.insert $ Tm.Member tr ur Tm.modeCreator
    opCreateMessage_ ur tr $ MsgUserJoined Tm.modeCreator
    return newTopic

checkedFailure :: Bool -> Ur.UserRef -> Tp.TopicRef -> OpError
checkedFailure True _ = IdInUse
checkedFailure False ur = GenerateIdFailed ur . (:[]) . Tp.topicRefId

inviteToTopic :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> StorableJson -> m (Either OpError Msg.Message)
inviteToTopic caller tr ur body = runOp $ do
    newMember <- testTopicPerm caller tr (const Tm.mmInvite) InviteUser >>= insertNewMember
    user <- GH.getBy ur >>= getOrThrow (UserNotFound ur)
    let newMode = Tm.memberMode newMember
        invitesRef = Tp.fromUserRef (Ur.userInvite user) ur
    testJoinTopicPerm_ caller invitesRef Tm.canSend SendMessage
    opCreateMessage_ caller invitesRef $ MsgInvitation tr newMode body
    opCreateMessage caller tr $ MsgInvitedUser ur invitesRef newMode
    where insertNewMember = opInsertMember ur tr . Tm.invitedMode . Tp.topicMode >=> throwEitherConst (AlreadyMember tr ur)
        
-- | ensures the caller is joined to the topic, and then performs the same
-- action as testTopicPerm
testJoinTopicPerm :: (MonadThrow m, MonadIO m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m Tp.Topic
testJoinTopicPerm caller tr modeP op = do
    topic <- opGetTopic tr
    mode <- opJoinTopic' caller tr topic
    unless (modeP (Tp.topicMode topic) mode) $ throwM (OperationDenied op)
    return topic

-- | unitary version of testJoinTopicPerm
testJoinTopicPerm_ :: (MonadThrow m, Functor m, MonadIO m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m ()
testJoinTopicPerm_ caller tr modeP op = void $ testJoinTopicPerm caller tr modeP op

joinTopic :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> m (Either OpError Tm.MemberMode)
joinTopic caller tr = runOp $ opJoinTopic caller tr

opJoinTopic :: (MonadThrow m, MonadIO m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> m Tm.MemberMode
opJoinTopic cr tr = opGetTopic tr >>= opJoinTopic' cr tr

opJoinTopic' :: (MonadThrow m, MonadIO m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Tp.Topic -> m Tm.MemberMode
opJoinTopic' cr tr = opInsertMember cr tr . Tm.joinerMode . Tp.topicMode >=> either (return . Tm.memberMode) (sendJoinMessage . Tm.memberMode) 
    where sendJoinMessage mode = (opCreateMessage cr tr $ MsgUserJoined mode) >> return mode

-- | attempts to insert a member (and return it on the right side). if the member already exists, returns it on the left side.
opInsertMember :: (MonadThrow m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Tm.MemberMode -> m (Either Tm.Member Tm.Member)
opInsertMember ur tr mode =  GH.getBy (Tm.TargetMemberKey tr ur) >>= maybe (insertMem newMember) (return . Left)
    where newMember = Tm.Member tr ur mode 
          insertMem m = GH.insert m >> return (Right m)

sendMessage :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> StorableJson -> m (Either OpError Message)
sendMessage caller tr body = runOp $ do
    testTopicPerm_ caller tr Tm.canSend SendMessage 
    opCreateMessage caller tr $ MsgPosted body

opLoadHandle :: (MonadThrow m, GH.PersistBackend m) => MsgHandle -> m Message
opLoadHandle (MsgHandle tr mid sender k) = GH.get k >>= maybe failure success
    where failure = throwM (LoadMessageFailed (MessageCoordKey tr mid))
          success = return . Message tr mid sender

opGetMessageHandle :: (MonadThrow m, GH.PersistBackend m) => MessageRef -> m MsgHandle
opGetMessageHandle mr = GH.getBy mr >>= getOrThrow (MessageNotFound mr)

getFromEnd :: CatchDbConn m cm conn => Bool -> Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Message])
getFromEnd forward caller tr n = runOp $ do
    testTopicPerm_ caller tr (const Tm.mmRead) ReadTopic
    let dir = bool GH.Desc GH.Asc forward
    handles <- GH.select $ (MhTopicField ==. tr) `GH.orderBy` [dir GH.AutoKeyField] `GH.limitTo` n
    mapM opLoadHandle handles

getFirst :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Message])
getFirst = getFromEnd True 

getLast :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Message])
getLast = getFromEnd False

getFromId :: CatchDbConn m cm conn => Bool -> Bool -> Ur.UserRef -> Tp.TopicRef -> MessageId -> Int -> m (Either OpError [Message])
getFromId forward inclusive caller tr mid n = runOp $ do
    testTopicPerm_ caller tr (const Tm.mmRead) ReadTopic
    ks <- GH.project GH.AutoKeyField $ (MhTopicField ==. tr) &&. (MhIdField ==. mid) 
    k <- getOrThrow (MessageNotFound $ MessageCoordKey tr mid) $ headMay ks
    handles <- GH.select $ ((MhTopicField ==. tr) &&. (GH.AutoKeyField `comp` k)) `GH.orderBy` [dir GH.AutoKeyField] `GH.limitTo` n
    mapM opLoadHandle handles
    where dir = bool GH.Desc GH.Asc forward
          selectComp True True = (GH.>=.)
          selectComp True False = (GH.>.)
          selectComp False True = (GH.<=.)
          selectComp False False = (GH.<.)
          comp = selectComp forward inclusive

infixr 9 .-
(.-) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-) = (.) . (.)

--use this only in a transaction
opSaveMessage :: (MonadThrow m, GH.PersistBackend m) => Message -> m Message
opSaveMessage (Message tr mid sender content) = opInsertMessage sender tr content (MessageIdInUse .- MessageCoordKey) mid

opCreateMessage :: (MonadThrow m, MonadIO m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> MsgContent -> m Message
opCreateMessage sender tr content = genRandom >>= opInsertMessage sender tr content ((. pure) . GenerateMessageIdFailed)

opCreateMessage_ :: (MonadThrow m, Functor m, MonadIO m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> MsgContent -> m ()
opCreateMessage_ sender tr content = void $ opCreateMessage sender tr content

opInsertMessage :: (MonadThrow m, GH.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> MsgContent -> (Tp.TopicRef -> MessageId -> OpError) -> MessageId -> m Message
opInsertMessage sender tr content err mid = do
    contentId <- GH.insert content
    res <- GH.insertByAll $ MsgHandle tr mid sender contentId
    when (isLeft res) $ throwM $ err tr mid
    return $ Message tr mid sender content
