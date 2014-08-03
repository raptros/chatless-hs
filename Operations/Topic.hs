module Operations.Topic where

import Operations.Base

import Model.ID
import Model.User
import Model.Topic
import Model.TopicMember
import Control.Monad.Except
import Database.Groundhog
import Control.Applicative
import Control.Monad.Random
--import Database.Groundhog.Generic (runDb)
--import Database.Groundhog.Core (ConnectionManager(..))
--
--todo wrap a bunch of these up in transactions.

getMemberEffectiveMode :: (PersistBackend m, Functor m) => UserRef -> Topic -> m MemberMode
getMemberEffectiveMode ur topic
    | ur `isCreator` topic = return modeCreator
    | otherwise = maybe (nonMemberMode $ topicMode topic) memberMode <$> query
        where query = getBy $ TargetMemberKey (extractUnique topic) ur


setMemberModeOp' :: (PersistBackend m, Functor m) => UserRef -> MemberMode -> TopicRef -> UserRef -> ExceptT ApiError m ()
setMemberModeOp' targetUser newMode tr callerRef = do
    topic <- (lift $ getBy tr) >>= (getOrThrow $ TopicNotFound tr)
    em <- lift $ getMemberEffectiveMode callerRef topic
    unless (mmSetMember em) $ throwError $ OperationDenied SetMemberMode
    targetMember <- (lift $ getBy $ TargetMemberKey tr targetUser) >>= (getOrThrow $ MemberNotFound tr targetUser)
    lift $ update [MemberModeField =. newMode] $ (MemberTopicField ==. tr) &&. (MemberUserField ==. targetUser)
    return ()

--listTopics :: (HasConn m cm conn, PersistBackend (DbPersist conn m)) => ServerId -> UserId -> m [TopicRef]
--listTopics tid uid = runDb $ project TopicCoord $ (TopicServerField ==. tid) &&. (TopicUserField ==. uid)

listTopicsOp :: PersistBackend m => UserRef -> m [TopicRef]
listTopicsOp ref = project TopicCoord $ (TopicServerField ==. userRefServer ref) &&. (TopicUserField ==. userRefUser ref)

createTopicOp :: (PersistBackend m) => UserRef -> TopicCreate -> ExceptT ApiError m Topic
createTopicOp ur tc = do
    --todo random topic ids
    tid <- getOrThrow (GenerateIdFailed ur [])
    let newTopic = createTopic ur tid tc
        firstMember = Member (tid `fromUserRef` ur) ur modeCreator
    --create a topic and insert the creator
    tcRes <- insertByAll newTopic
    --if topic insertion did not fail constraints, neither should first member insertion
    insertByAll firstMember
    eitherConst (sendResponseStatus status400 $ reasonObject "id_in_use" []) (returnJson newTopic) r

initializeTopic :: UserRef -> TopicId -> TopicCreate -> Topic
initializeTopic caller tid (TopicCreate _ mBanner mInfo mMode) = Topic {
    topicServer = userRefServer caller,
    topicUser = userRefUser caller,
    topicId = tid,
    topicBanner = fromMaybe "" mBanner,
    topicInfo = fromMaybe storableEmpty mInfo,
    topicMode = fromMaybe defaultTopicMode mMode
}

