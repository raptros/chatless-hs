{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.Topic where

import Operations.Base

import Model.ID
import Model.User
import Model.Topic
import Model.TopicMember
import Model.StorableJson
import Control.Monad.Except
import Control.Monad.Catch
import Database.Groundhog
import Database.Groundhog.Generic
import Control.Applicative
import Control.Monad.Random
import Data.Maybe
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

setMemberModeOp'' :: (CatchDbConn m cm conn, Functor m) => UserRef -> MemberMode -> TopicRef -> UserRef -> m (Either ApiError ())
setMemberModeOp'' targetUser newMode tr callerRef = runOp $ do
    topic <- getBy tr >>= (getOrThrow' $ TopicNotFound tr)
    em <- getMemberEffectiveMode callerRef topic
    unless (mmSetMember em) $ throwM $ OperationDenied SetMemberMode
    targetMember <- (getBy $ TargetMemberKey tr targetUser) >>= (getOrThrow' $ MemberNotFound tr targetUser)
    update [MemberModeField =. newMode] $ (MemberTopicField ==. tr) &&. (MemberUserField ==. targetUser)
    return ()

--listTopics :: (HasConn m cm conn, PersistBackend (DbPersist conn m)) => ServerId -> UserId -> m [TopicRef]
--listTopics tid uid = runDb $ project TopicCoord $ (TopicServerField ==. tid) &&. (TopicUserField ==. uid)

listTopicsOp :: PersistBackend m => UserRef -> m [TopicRef]
listTopicsOp ref = project TopicCoord $ (TopicServerField ==. userRefServer ref) &&. (TopicUserField ==. userRefUser ref)

{-
createTopicOp :: (Functor m, PersistBackend m) => UserRef -> TopicCreate -> ExceptT ApiError m Topic
createTopicOp ur tc = do
    --todo random topic ids
    tid <- getOrThrow (GenerateIdFailed ur []) (createId tc)
    let newTopic = initializeTopic ur tid tc
        firstMember = Member (tid `fromUserRef` ur) ur modeCreator
    --create a topic and insert the creator
    tcRes <- withExceptT (const $ IdInUse $ fromUserRef tid ur) (ExceptT $ insertByAll newTopic)
    --if topic insertion did not fail constraints, neither should first member insertion
    lift $ insert firstMember
    return newTopic
    --eitherConst (sendResponseStatus status400 $ reasonObject "id_in_use" []) (returnJson newTopic) r
    -}

createTopicOp :: CatchDbConn m cm conn => UserRef -> TopicCreate -> m (Either ApiError Topic)
createTopicOp ur tc = runOp $ do
    tid <- getOrThrow' (GenerateIdFailed ur []) (createId tc)
    let tr = fromUserRef tid ur
        newTopic = initializeTopic ur tid tc
        firstMember = Member tr ur modeCreator
    --create a topic and insert the creator
    tcRes <- insertByAll newTopic
    throwEitherConst (IdInUse tr) tcRes
    insert firstMember
    return newTopic



initializeTopic :: UserRef -> TopicId -> TopicCreate -> Topic
initializeTopic caller tid (TopicCreate _ mBanner mInfo mMode) = Topic {
    topicServer = userRefServer caller,
    topicUser = userRefUser caller,
    topicId = tid,
    topicBanner = fromMaybe "" mBanner,
    topicInfo = fromMaybe storableEmpty mInfo,
    topicMode = fromMaybe defaultTopicMode mMode
}

