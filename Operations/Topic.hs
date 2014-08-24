{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.Topic where

import Safe (headMay)
import Control.Monad (when, unless, (>=>), void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Database.Groundhog as Gh
import Control.Applicative (pure, (<$>))
import Data.Maybe (isJust)
import Data.Either (isLeft)
import Data.Bool (bool)

import Model.StorableJson (StorableJson)
import Model.IDGen (genRandom)
import Model.ID (ServerId, UserId, TopicId, MessageId)
import qualified Model.User as Ur
import qualified Model.Topic as Tp
import qualified Model.TopicMember as Tm
import qualified Model.Message as Msg

import Operations.Base (runOp, CatchDbConn, OpError(..), OpType(..), getOrThrow, throwEitherConst)

getTopic :: CatchDbConn m cm conn => Tp.TopicRef -> m (Either OpError Tp.Topic)
getTopic = runOp . opGetTopic

setTopicMode :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Tp.TopicModeUpdate -> m (Either OpError Msg.Message)
setTopicMode caller tr tmu = runOp $ do
    topic <- testTopicPerm caller tr (const Tm.mmSetMode) SetTopicMode
    let newMode = Tp.resolveTopicModeUpdate (Tp.topicMode topic) tmu
    Gh.update [Tp.TopicModeField Gh.=. newMode] $ Tp.TopicCoord Gh.==. tr
    opCreateMessage caller tr $ Msg.MsgTopicModeChanged newMode

listMembers :: (CatchDbConn m cm conn, Functor m) => Ur.UserRef -> Tp.TopicRef -> m (Either OpError [Tm.MemberPartial])
listMembers callerRef tr = runOp $ do
    testTopicPerm_ callerRef tr (const Tm.mmRead) ReadTopic
    res <- Gh.project (Tm.MemberUserField, Tm.MemberModeField) (Tm.MemberTopicField Gh.==. tr)
    return $ uncurry Tm.MemberPartial <$> res

getMember :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> m (Either OpError Tm.MemberMode)
getMember callerRef tr ur = runOp $ do
    testTopicPerm_ callerRef tr (const Tm.mmRead) ReadTopic
    res <- Gh.getBy (Tm.TargetMemberKey tr ur) 
    getOrThrow (MemberNotFound tr ur) (Tm.memberMode <$> res)

opGetTopic :: (Gh.PersistBackend m, MonadThrow m) => Tp.TopicRef -> m Tp.Topic
opGetTopic tr = Gh.getBy tr >>= getOrThrow (TopicNotFound tr)

--todo figure out best order of arguments
setMemberMode :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> Tm.MemberModeUpdate -> m (Either OpError Msg.Message)
setMemberMode callerRef tr targetUser mmu = runOp $ do
    testTopicPerm_ callerRef tr (const Tm.mmSetMember) SetMemberMode
    targetMember <- Gh.getBy (Tm.TargetMemberKey tr targetUser) >>= getOrThrow (MemberNotFound tr targetUser)
    let newMode = Tm.resolveMemberModeUpdate (Tm.memberMode targetMember) mmu
    Gh.update [Tm.MemberModeField Gh.=. newMode] $ (Tm.MemberTopicField Gh.==. tr) Gh.&&. (Tm.MemberUserField Gh.==. targetUser)
    opCreateMessage callerRef tr $ Msg.MsgMemberModeChanged targetUser newMode

testTopicPerm :: (MonadThrow m, Functor m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m Tp.Topic
testTopicPerm caller tr modeP op = do
    topic <- opGetTopic tr
    em <- getMemberEffectiveMode caller topic
    unless (modeP (Tp.topicMode topic) em) $ throwM (OperationDenied op)
    return topic

getMemberEffectiveMode :: (Functor m, Gh.PersistBackend m) => Ur.UserRef -> Tp.Topic -> m Tm.MemberMode
getMemberEffectiveMode ur topic
    | ur `Tp.isCreator` topic = return Tm.modeCreator
    | otherwise =  extract <$> query 
        where query = Gh.getBy $ Tm.TargetMemberKey (Gh.extractUnique topic) ur
              extract = maybe (Tm.nonMemberMode (Tp.topicMode topic)) Tm.memberMode

testTopicPerm_ :: (MonadThrow m, Functor m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m ()
testTopicPerm_ caller tr mode op = void $ testTopicPerm caller tr mode op

createTopic :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicCreate -> m (Either OpError Tp.Topic)
createTopic ur tc = runOp $ do 
    tid <- maybe genRandom return $ Tp.createId tc
    let tr = Tp.fromUserRef tid ur
        newTopic = Tp.initializeTopic ur tid tc
        givenId = isJust $ Tp.createId tc
    --create a topic and insert the creator
    tcRes <- Gh.insertByAll newTopic
    when (isLeft tcRes) $ throwM $ checkedFailure givenId ur tr
    Gh.insert $ Tm.Member tr ur Tm.modeCreator
    opCreateMessage_ ur tr $ Msg.MsgUserJoined Tm.modeCreator
    return newTopic

checkedFailure :: Bool -> Ur.UserRef -> Tp.TopicRef -> OpError
checkedFailure True _ = IdInUse
checkedFailure False ur = GenerateIdFailed ur . (:[]) . Tp.topicRefId

inviteToTopic :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> StorableJson -> m (Either OpError Msg.Message)
inviteToTopic caller tr ur body = runOp $ do
    newMember <- testTopicPerm caller tr (const Tm.mmInvite) InviteUser >>= insertNewMember
    user <- Gh.getBy ur >>= getOrThrow (UserNotFound ur)
    let newMode = Tm.memberMode newMember
        invitesRef = Tp.fromUserRef (Ur.userInvite user) ur
    testJoinTopicPerm_ caller invitesRef Tm.canSend SendMessage
    opCreateMessage_ caller invitesRef $ Msg.MsgInvitation tr newMode body
    opCreateMessage caller tr $ Msg.MsgInvitedUser ur invitesRef newMode
    where insertNewMember = opInsertMember ur tr . Tm.invitedMode . Tp.topicMode >=> throwEitherConst (AlreadyMember tr ur)
        
-- | ensures the caller is joined to the topic, and then performs the same
-- action as testTopicPerm
testJoinTopicPerm :: (MonadThrow m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m Tp.Topic
testJoinTopicPerm caller tr modeP op = do
    topic <- opGetTopic tr
    mode <- opJoinTopic' caller tr topic
    unless (modeP (Tp.topicMode topic) mode) $ throwM (OperationDenied op)
    return topic

-- | unitary version of testJoinTopicPerm
testJoinTopicPerm_ :: (MonadThrow m, Functor m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m ()
testJoinTopicPerm_ caller tr modeP op = void $ testJoinTopicPerm caller tr modeP op

joinTopic :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> m (Either OpError Tm.MemberMode)
joinTopic caller tr = runOp $ opJoinTopic caller tr

opJoinTopic :: (MonadThrow m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> m Tm.MemberMode
opJoinTopic cr tr = opGetTopic tr >>= opJoinTopic' cr tr

opJoinTopic' :: (MonadThrow m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Tp.Topic -> m Tm.MemberMode
opJoinTopic' cr tr = opInsertMember cr tr . Tm.joinerMode . Tp.topicMode >=> either (return . Tm.memberMode) (sendJoinMessage . Tm.memberMode) 
    where sendJoinMessage mode = (opCreateMessage cr tr $ Msg.MsgUserJoined mode) >> return mode

-- | attempts to insert a member (and return it on the right side). if the member already exists, returns it on the left side.
opInsertMember :: (MonadThrow m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Tm.MemberMode -> m (Either Tm.Member Tm.Member)
opInsertMember ur tr mode =  Gh.getBy (Tm.TargetMemberKey tr ur) >>= maybe (insertMem newMember) (return . Left)
    where newMember = Tm.Member tr ur mode 
          insertMem m = Gh.insert m >> return (Right m)

sendMessage :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> StorableJson -> m (Either OpError Msg.Message)
sendMessage caller tr body = runOp $ do
    testTopicPerm_ caller tr Tm.canSend SendMessage 
    opCreateMessage caller tr $ Msg.MsgPosted body

opLoadHandle :: (MonadThrow m, Gh.PersistBackend m) => Msg.MsgHandle -> m Msg.Message
opLoadHandle (Msg.MsgHandle tr mid sender k) = Gh.get k >>= maybe failure success
    where failure = throwM (LoadMessageFailed (Msg.MessageCoordKey tr mid))
          success = return . Msg.Message tr mid sender

opGetMessageHandle :: (MonadThrow m, Gh.PersistBackend m) => Msg.MessageRef -> m Msg.MsgHandle
opGetMessageHandle mr = Gh.getBy mr >>= getOrThrow (MessageNotFound mr)

getFromEnd :: CatchDbConn m cm conn => Bool -> Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Msg.Message])
getFromEnd forward caller tr n = runOp $ do
    testTopicPerm_ caller tr (const Tm.mmRead) ReadTopic
    let dir = bool Gh.Desc Gh.Asc forward
    handles <- Gh.select $ (Msg.MhTopicField Gh.==. tr) `Gh.orderBy` [dir Gh.AutoKeyField] `Gh.limitTo` n
    mapM opLoadHandle handles

getFirst :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Msg.Message])
getFirst = getFromEnd True 

getLast :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Msg.Message])
getLast = getFromEnd False

getFromId :: CatchDbConn m cm conn => Bool -> Bool -> Ur.UserRef -> Tp.TopicRef -> MessageId -> Int -> m (Either OpError [Msg.Message])
getFromId forward inclusive caller tr mid n = runOp $ do
    testTopicPerm_ caller tr (const Tm.mmRead) ReadTopic
    ks <- Gh.project Gh.AutoKeyField $ (Msg.MhTopicField Gh.==. tr) Gh.&&. (Msg.MhIdField Gh.==. mid) 
    k <- getOrThrow (MessageNotFound $ Msg.MessageCoordKey tr mid) $ headMay ks
    handles <- Gh.select $ ((Msg.MhTopicField Gh.==. tr) Gh.&&. (Gh.AutoKeyField `comp` k)) `Gh.orderBy` [dir Gh.AutoKeyField] `Gh.limitTo` n
    mapM opLoadHandle handles
    where dir = bool Gh.Desc Gh.Asc forward
          selectComp True True = (Gh.>=.)
          selectComp True False = (Gh.>.)
          selectComp False True = (Gh.<=.)
          selectComp False False = (Gh.<.)
          comp = selectComp forward inclusive

infixr 9 .-
(.-) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-) = (.) . (.)

--use this only in a transaction
opSaveMessage :: (MonadThrow m, Gh.PersistBackend m) => Msg.Message -> m Msg.Message
opSaveMessage (Msg.Message tr mid sender content) = opInsertMessage sender tr content (MessageIdInUse .- Msg.MessageCoordKey) mid

opCreateMessage :: (MonadThrow m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Msg.MsgContent -> m Msg.Message
opCreateMessage sender tr content = genRandom >>= opInsertMessage sender tr content ((. pure) . GenerateMessageIdFailed)

opCreateMessage_ :: (MonadThrow m, Functor m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Msg.MsgContent -> m ()
opCreateMessage_ sender tr content = void $ opCreateMessage sender tr content

opInsertMessage :: (MonadThrow m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Msg.MsgContent -> (Tp.TopicRef -> MessageId -> OpError) -> MessageId -> m Msg.Message
opInsertMessage sender tr content err mid = do
    contentId <- Gh.insert content
    res <- Gh.insertByAll $ Msg.MsgHandle tr mid sender contentId
    when (isLeft res) $ throwM $ err tr mid
    return $ Msg.Message tr mid sender content
