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
import Database.Groundhog.Generic
import Control.Applicative
import Control.Monad.Random
import Data.Maybe
import Data.Either
import Data.Bool
import Model.IDGen
import Control.Lens

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
    testTopicPerm callerRef tr (const mmRead) ReadTopic
    res <- project (MemberUserField, MemberModeField) (MemberTopicField ==. tr)
    return $ uncurry MemberPartial <$> res


getMember :: (CatchDbConn m cm conn) => UserRef -> TopicRef -> UserRef -> m (Either OpError MemberMode)
getMember callerRef tr ur = runOp $ do
    testTopicPerm callerRef tr (const mmRead) ReadTopic
    res <- getBy (TargetMemberKey tr ur) 
    getOrThrow (MemberNotFound tr ur) (memberMode <$> res)

opGetTopic :: (PersistBackend m, MonadThrow m) => TopicRef -> m Topic
opGetTopic tr = getBy tr >>= getOrThrow (TopicNotFound tr)

--todo figure out best order of arguments
setMemberMode :: (CatchDbConn m cm conn, Functor m) => UserRef -> TopicRef -> UserRef -> MemberModeUpdate -> m (Either OpError Message)
setMemberMode callerRef tr targetUser modeUpdate = runOp $ do
    testTopicPerm callerRef tr (const mmSetMember) SetMemberMode
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

createTopic :: (CatchDbConn m cm conn) => UserRef -> TopicCreate -> m (Either OpError Topic)
createTopic ur tc = runOp $ do 
    tid <- maybe genRandom return $ createId tc
    let tr = fromUserRef tid ur
        newTopic = initializeTopic ur tid tc
        firstMember = Member tr ur modeCreator
        givenId = isJust $ createId tc
    --create a topic and insert the creator
    tcRes <- insertByAll newTopic
    when (isLeft tcRes) $ throwM $ checkedFailure givenId ur tr
    insert firstMember
    --insert a message
    return newTopic

checkedFailure :: Bool -> UserRef -> TopicRef -> OpError
checkedFailure True _ = IdInUse
checkedFailure False ur = GenerateIdFailed ur . (:[]) . topicRefId

joinTopic :: CatchDbConn m cm conn => UserRef -> TopicRef -> m (Either OpError MemberMode)
joinTopic caller tr = runOp $ do
    mode <- joinTopicOp caller tr
    opCreateMessage caller tr $ MsgUserJoined mode
    return mode

joinTopicOp :: (MonadThrow m, PersistBackend m) => UserRef -> TopicRef -> m MemberMode
joinTopicOp cr tr = do
    topic <- opGetTopic tr
    member <- getBy (TargetMemberKey tr cr) >>= maybe (insertMember topic) return
    return $ memberMode member
    where memberForTopic = Member tr cr . joinerMode . topicMode
          insertMember = monadKestrel insert . memberForTopic

monadKestrel :: (Monad m) => (a -> m b) -> a -> m a
monadKestrel f a = f a >> return a 

sendMessage :: CatchDbConn m cm conn => UserRef -> TopicRef -> StorableJson -> m (Either OpError Message)
sendMessage caller tr body = runOp $ do
    testTopicPerm caller tr canSend SendMessage 
    opCreateMessage caller tr $ MsgPosted body

opLoadHandle :: (MonadThrow m, PersistBackend m) => MsgHandle -> m Message
opLoadHandle (MsgHandle tr id sender k) = get k >>= maybe failure success
    where failure = throwM $ LoadMessageFailed $ MessageCoordKey tr id
          success = return . Message tr id sender

opGetMessageHandle :: (MonadThrow m, PersistBackend m) => MessageRef -> m MsgHandle
opGetMessageHandle mr = getBy mr >>= getOrThrow (MessageNotFound mr)

getFromEnd :: CatchDbConn m cm conn => Bool -> UserRef -> TopicRef -> Int -> m (Either OpError [Message])
getFromEnd forward caller tr count = runOp $ do
    testTopicPerm caller tr (const mmRead) ReadTopic
    let dir = bool Desc Asc forward
    handles <- select $ (MhTopicField ==. tr) `orderBy` [dir AutoKeyField] `limitTo` count
    mapM opLoadHandle handles

getFirst :: CatchDbConn m cm conn => UserRef -> TopicRef -> Int -> m (Either OpError [Message])
getFirst = getFromEnd True 

getLast :: CatchDbConn m cm conn => UserRef -> TopicRef -> Int -> m (Either OpError [Message])
getLast = getFromEnd False

getFromId :: CatchDbConn m cm conn => Bool -> Bool -> UserRef -> TopicRef -> MessageId -> Int -> m (Either OpError [Message])
getFromId forward inclusive caller tr id count = runOp $ do
    ks <- project AutoKeyField $ (MhTopicField ==. tr) &&. (MhIdField ==. id) 
    k <- getOrThrow (MessageNotFound $ MessageCoordKey tr id) $ headMay ks
    handles <- select $ ((MhTopicField ==. tr) &&. (AutoKeyField `comp` k)) `orderBy` [dir AutoKeyField] `limitTo` count
    mapM opLoadHandle handles
    where dir = bool Desc Asc forward
          comp
            | forward && inclusive = (>=.)
            | forward && not inclusive = (>.)
            | not forward && inclusive = (<=.)
            | not forward && not inclusive = (Database.Groundhog.<.)

infixr 9 .-
(.-) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.-) = (.) . (.)

--use this only in a transaction
opSaveMessage :: (MonadThrow m, PersistBackend m) => Message -> m Message
opSaveMessage (Message tr id sender content) = opInsertMessage sender tr content (MessageIdInUse .- MessageCoordKey) id

opCreateMessage :: (MonadThrow m, MonadIO m, PersistBackend m) => UserRef -> TopicRef -> MsgContent -> m Message
opCreateMessage sender tr content = genRandom >>= opInsertMessage sender tr content ((. pure) . GenerateMessageIdFailed)

opInsertMessage :: (MonadThrow m, PersistBackend m) => UserRef -> TopicRef -> MsgContent -> (TopicRef -> MessageId -> OpError) -> MessageId -> m Message
opInsertMessage sender tr content err id = do
    contentId <- insert content
    res <- insertByAll $ MsgHandle tr id sender contentId
    when (isLeft res) $ throwM $ err tr id
    return $ Message tr id sender content
