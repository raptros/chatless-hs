{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.Topic where

import Operations.Base

import Safe
import Model.ID
import Model.User
import Model.Topic
import Model.TopicMember
import Model.StorableJson
import Model.Message
import Control.Monad
import Control.Monad.Except
import Control.Monad.Catch
import Database.Groundhog
import Database.Groundhog.Generic()
import Control.Applicative
import Data.Maybe
import Data.Either
import Data.Bool
import Model.IDGen

getTopic :: CatchDbConn m cm conn => TopicRef -> m (Either OpError Topic)
getTopic = runOp . opGetTopic

setTopicMode :: CatchDbConn m cm conn => UserRef -> TopicRef -> TopicModeUpdate -> m (Either OpError Message)
setTopicMode caller tr tmu = runOp $ do
    topic <- testTopicPerm caller tr (const mmSetMode) SetTopicMode
    let newMode = resolveTopicModeUpdate (topicMode topic) tmu
    update [TopicModeField =. newMode] $ TopicCoord ==. tr
    opCreateMessage caller tr $ MsgTopicModeChanged newMode

getMemberEffectiveMode :: PersistBackend m => UserRef -> Topic -> m MemberMode
getMemberEffectiveMode ur topic
    | ur `isCreator` topic = return modeCreator
    | otherwise = liftM extract query --todo replace with fmap once monads are all functors
        where query = getBy $ TargetMemberKey (extractUnique topic) ur
              extract = maybe (nonMemberMode (topicMode topic)) memberMode

listMembers :: (CatchDbConn m cm conn, Functor m) => UserRef -> TopicRef -> m (Either OpError [MemberPartial])
listMembers callerRef tr = runOp $ do
    testTopicPerm_ callerRef tr (const mmRead) ReadTopic
    res <- project (MemberUserField, MemberModeField) (MemberTopicField ==. tr)
    return $ uncurry MemberPartial <$> res

getMember :: (CatchDbConn m cm conn) => UserRef -> TopicRef -> UserRef -> m (Either OpError MemberMode)
getMember callerRef tr ur = runOp $ do
    testTopicPerm_ callerRef tr (const mmRead) ReadTopic
    res <- getBy (TargetMemberKey tr ur) 
    getOrThrow (MemberNotFound tr ur) (memberMode <$> res)

opGetTopic :: (PersistBackend m, MonadThrow m) => TopicRef -> m Topic
opGetTopic tr = getBy tr >>= getOrThrow (TopicNotFound tr)

--todo figure out best order of arguments
setMemberMode :: (CatchDbConn m cm conn, Functor m) => UserRef -> TopicRef -> UserRef -> MemberModeUpdate -> m (Either OpError Message)
setMemberMode callerRef tr targetUser modeUpdate = runOp $ do
    testTopicPerm_ callerRef tr (const mmSetMember) SetMemberMode
    targetMember <- getBy (TargetMemberKey tr targetUser) >>= getOrThrow (MemberNotFound tr targetUser)
    let newMode = resolveMemberModeUpdate (memberMode targetMember) modeUpdate
    update [MemberModeField =. newMode] $ (MemberTopicField ==. tr) &&. (MemberUserField ==. targetUser)
    opCreateMessage callerRef tr $ MsgMemberModeChanged targetUser newMode

testTopicPerm :: (MonadThrow m, PersistBackend m) => UserRef -> TopicRef -> (TopicMode -> MemberMode -> Bool) -> OpType -> m Topic
testTopicPerm caller tr mode op = do
    topic <- opGetTopic tr
    em <- getMemberEffectiveMode caller topic
    unless (mode (topicMode topic) em) $ throwM (OperationDenied op)
    return topic

testTopicPerm_ :: (MonadThrow m, PersistBackend m) => UserRef -> TopicRef -> (TopicMode -> MemberMode -> Bool) -> OpType -> m ()
testTopicPerm_ caller tr mode op = testTopicPerm caller tr mode op >> return ()

createTopic :: (CatchDbConn m cm conn) => UserRef -> TopicCreate -> m (Either OpError Topic)
createTopic ur tc = runOp $ do 
    tid <- maybe genRandom return $ createId tc
    let tr = fromUserRef tid ur
        newTopic = initializeTopic ur tid tc
        givenId = isJust $ createId tc
    --create a topic and insert the creator
    tcRes <- insertByAll newTopic
    when (isLeft tcRes) $ throwM $ checkedFailure givenId ur tr
    insert $ Member tr ur modeCreator
    _ <- opCreateMessage ur tr $ MsgUserJoined modeCreator
    return newTopic

checkedFailure :: Bool -> UserRef -> TopicRef -> OpError
checkedFailure True _ = IdInUse
checkedFailure False ur = GenerateIdFailed ur . (:[]) . topicRefId

inviteToTopic :: CatchDbConn m cm conn => UserRef -> TopicRef -> UserRef -> StorableJson -> m (Either OpError Message)
inviteToTopic caller tr ur body = runOp $ do
    newMember <- testTopicPerm caller tr (const mmInvite) InviteUser >>= insertNewMember
    user <- getBy ur >>= getOrThrow (UserNotFound ur)
    let newMode = memberMode newMember
    let invitesRef = fromUserRef (userInvite user) ur
    testJoinTopicPerm_ caller invitesRef canSend SendMessage
    opCreateMessage_ caller invitesRef $ MsgInvitation tr newMode body
    opCreateMessage caller tr $ MsgInvitedUser ur invitesRef newMode
    where insertNewMember = opInsertMember ur tr . invitedMode . topicMode >=> throwEitherConst (AlreadyMember tr ur)
        
-- | ensures the caller is joined to the topic, and then performs the same
-- action as testTopicPerm
testJoinTopicPerm :: (MonadThrow m, MonadIO m, PersistBackend m) => UserRef -> TopicRef -> (TopicMode -> MemberMode -> Bool) -> OpType -> m Topic
testJoinTopicPerm caller tr modeP op = do
    topic <- opGetTopic tr
    mode <- opJoinTopic' caller tr topic
    unless (modeP (topicMode topic) mode) $ throwM (OperationDenied op)
    return topic

-- | unitary version of testJoinTopicPerm
testJoinTopicPerm_ :: (MonadThrow m, MonadIO m, PersistBackend m) => UserRef -> TopicRef -> (TopicMode -> MemberMode -> Bool) -> OpType -> m ()
testJoinTopicPerm_ caller tr modeP op = testJoinTopicPerm caller tr modeP op >> return ()

joinTopic :: CatchDbConn m cm conn => UserRef -> TopicRef -> m (Either OpError MemberMode)
joinTopic caller tr = runOp $ opJoinTopic caller tr

opJoinTopic :: (MonadThrow m, MonadIO m, PersistBackend m) => UserRef -> TopicRef -> m MemberMode
opJoinTopic cr tr = opGetTopic tr >>= opJoinTopic' cr tr

opJoinTopic' :: (MonadThrow m, MonadIO m, PersistBackend m) => UserRef -> TopicRef -> Topic -> m MemberMode
opJoinTopic' cr tr = opInsertMember cr tr . joinerMode . topicMode >=> either (return . memberMode) (sendJoinMessage . memberMode) 
    where sendJoinMessage mode = (opCreateMessage cr tr $ MsgUserJoined mode) >> return mode

-- | attempts to insert a member (and return it on the right side). if the member already exists, returns it on the left side.
opInsertMember :: (MonadThrow m, PersistBackend m) => UserRef -> TopicRef -> MemberMode -> m (Either Member Member)
opInsertMember ur tr mode =  getBy (TargetMemberKey tr ur) >>= maybe (insertMem newMember) (return . Left)
    where newMember = Member tr ur mode 
          insertMem m = insert m >> return (Right m)

sendMessage :: CatchDbConn m cm conn => UserRef -> TopicRef -> StorableJson -> m (Either OpError Message)
sendMessage caller tr body = runOp $ do
    testTopicPerm_ caller tr canSend SendMessage 
    opCreateMessage caller tr $ MsgPosted body

opLoadHandle :: (MonadThrow m, PersistBackend m) => MsgHandle -> m Message
opLoadHandle (MsgHandle tr mid sender k) = get k >>= maybe failure success
    where failure = throwM $ LoadMessageFailed $ MessageCoordKey tr mid
          success = return . Message tr mid sender

opGetMessageHandle :: (MonadThrow m, PersistBackend m) => MessageRef -> m MsgHandle
opGetMessageHandle mr = getBy mr >>= getOrThrow (MessageNotFound mr)

getFromEnd :: CatchDbConn m cm conn => Bool -> UserRef -> TopicRef -> Int -> m (Either OpError [Message])
getFromEnd forward caller tr n = runOp $ do
    testTopicPerm_ caller tr (const mmRead) ReadTopic
    let dir = bool Desc Asc forward
    handles <- select $ (MhTopicField ==. tr) `orderBy` [dir AutoKeyField] `limitTo` n
    mapM opLoadHandle handles

getFirst :: CatchDbConn m cm conn => UserRef -> TopicRef -> Int -> m (Either OpError [Message])
getFirst = getFromEnd True 

getLast :: CatchDbConn m cm conn => UserRef -> TopicRef -> Int -> m (Either OpError [Message])
getLast = getFromEnd False

getFromId :: CatchDbConn m cm conn => Bool -> Bool -> UserRef -> TopicRef -> MessageId -> Int -> m (Either OpError [Message])
getFromId forward inclusive caller tr mid n = runOp $ do
    testTopicPerm_ caller tr (const mmRead) ReadTopic
    ks <- project AutoKeyField $ (MhTopicField ==. tr) &&. (MhIdField ==. mid) 
    k <- getOrThrow (MessageNotFound $ MessageCoordKey tr mid) $ headMay ks
    handles <- select $ ((MhTopicField ==. tr) &&. (AutoKeyField `comp` k)) `orderBy` [dir AutoKeyField] `limitTo` n
    mapM opLoadHandle handles
    where dir = bool Desc Asc forward
          selectComp True True = (>=.)
          selectComp True False = (>.)
          selectComp False True = (<=.)
          selectComp False False = (Database.Groundhog.<.)
          comp = selectComp forward inclusive

infixr 9 .-
(.-) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-) = (.) . (.)

--use this only in a transaction
opSaveMessage :: (MonadThrow m, PersistBackend m) => Message -> m Message
opSaveMessage (Message tr mid sender content) = opInsertMessage sender tr content (MessageIdInUse .- MessageCoordKey) mid

opCreateMessage_ :: (MonadThrow m, MonadIO m, PersistBackend m) => UserRef -> TopicRef -> MsgContent -> m ()
opCreateMessage_ sender tr content = opCreateMessage sender tr content >> return ()

opCreateMessage :: (MonadThrow m, MonadIO m, PersistBackend m) => UserRef -> TopicRef -> MsgContent -> m Message
opCreateMessage sender tr content = genRandom >>= opInsertMessage sender tr content ((. pure) . GenerateMessageIdFailed)

opInsertMessage :: (MonadThrow m, PersistBackend m) => UserRef -> TopicRef -> MsgContent -> (TopicRef -> MessageId -> OpError) -> MessageId -> m Message
opInsertMessage sender tr content err mid = do
    contentId <- insert content
    res <- insertByAll $ MsgHandle tr mid sender contentId
    when (isLeft res) $ throwM $ err tr mid
    return $ Message tr mid sender content
