{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.Topic where

import Operations.Base

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
import Model.IDGen

getMemberEffectiveMode :: PersistBackend m => UserRef -> Topic -> m MemberMode
getMemberEffectiveMode ur topic
    | ur `isCreator` topic = return modeCreator
    | otherwise = liftM extract query --todo replace with fmap once monads are all functors
        where query = getBy $ TargetMemberKey (extractUnique topic) ur
              extract = maybe (nonMemberMode (topicMode topic)) memberMode

listMembers :: (CatchDbConn m cm conn, Functor m) => UserRef -> TopicRef -> m (Either OpError [MemberPartial])
listMembers callerRef tr = runOp $ do
    testTopicPerm callerRef tr mmRead ReadTopic
    res <- project (MemberUserField, MemberModeField) $ MemberTopicField ==. tr
    return $ uncurry MemberPartial <$> res

getMember :: (CatchDbConn m cm conn) => UserRef -> TopicRef -> UserRef -> m (Either OpError MemberMode)
getMember callerRef tr ur = runOp $ do
    testTopicPerm callerRef tr mmRead ReadTopic
    res <- getBy (TargetMemberKey tr ur) 
    getOrThrow (MemberNotFound tr ur) (memberMode <$> res)

opGetTopic :: (PersistBackend m, MonadThrow m) => TopicRef -> m Topic
opGetTopic tr = getBy tr >>= getOrThrow (TopicNotFound tr)

--todo figure out best order of arguments
setMemberMode :: (CatchDbConn m cm conn, Functor m) => UserRef -> TopicRef -> UserRef -> MemberModeUpdate -> m (Either OpError Message)
setMemberMode callerRef tr targetUser modeUpdate = runOp $ do
    testTopicPerm callerRef tr mmSetMember SetMemberMode
    targetMember <- getBy (TargetMemberKey tr targetUser) >>= getOrThrow (MemberNotFound tr targetUser)
    let newMode = resolveMemberModeUpdate (memberMode targetMember) modeUpdate
    update [MemberModeField =. newMode] $ (MemberTopicField ==. tr) &&. (MemberUserField ==. targetUser)
    newMsgId <- genRandom  --declare victory and retreat
    let newMsg = Message tr newMsgId callerRef $ MsgMemberModeChanged targetUser newMode
    return newMsg

testTopicPerm :: (MonadThrow m, PersistBackend m) => UserRef -> TopicRef -> (MemberMode -> Bool) -> OpType -> m ()
testTopicPerm caller tr mode op = do
    topic <- opGetTopic tr
    em <- getMemberEffectiveMode caller topic
    unless (mode em) $ throwM (OperationDenied op)

listTopicsOp :: PersistBackend m => UserRef -> m [TopicRef]
listTopicsOp ref = project TopicCoord $ (TopicServerField ==. userRefServer ref) &&. (TopicUserField ==. userRefUser ref)

createTopic :: (CatchDbConn m cm conn) => UserRef -> TopicCreate -> m (Either OpError Topic)
createTopic ur tc = runOp $ do 
    tid <- maybe genRandom return $ createId tc
    let tr = fromUserRef tid ur
        newTopic = initializeTopic ur tid tc
        firstMember = Member tr ur modeCreator
    --create a topic and insert the creator
    tcRes <- insertByAll newTopic
    throwEitherConst (checkedFailure givenId ur tr) tcRes
    insert firstMember
    return newTopic
    where givenId = isJust . createId $ tc

checkedFailure :: Bool -> UserRef -> TopicRef -> OpError
checkedFailure True _ = IdInUse
checkedFailure False ur = GenerateIdFailed ur . (:[]) . topicRefId

joinTopic :: CatchDbConn m cm conn => UserRef -> TopicRef -> m (Either OpError MemberMode)
joinTopic = (runOp .) . joinTopicOp

joinTopicOp :: (MonadThrow m, PersistBackend m) => UserRef -> TopicRef -> m MemberMode
joinTopicOp cr tr = do
    topic <- opGetTopic tr
    member <- getBy (TargetMemberKey tr cr) >>= maybe (insertMember topic) return
    return $ memberMode member
    where memberForTopic = Member tr cr . joinerMode . topicMode
          insertMember = monadKestrel insert . memberForTopic

monadKestrel :: (Monad m) => (a -> m b) -> a -> m a
monadKestrel f a = f a >> return a 


