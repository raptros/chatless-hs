{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.Internal.Topic where

import Control.Monad (when, unless, (>=>), void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Database.Groundhog as Gh
import Control.Applicative (pure, (<$>))
import Data.Either (isLeft)

import Model.IDGen (genRandom)
import Model.ID (MessageId)
import qualified Model.User as Ur
import qualified Model.Topic as Tp
import qualified Model.TopicMember as Tm
import qualified Model.Message as Msg
import Utils ((.*), (.**), (.***))
import Operations.Base (OpError(..), OpType(..), getOrThrow)

opGetTopic :: (Gh.PersistBackend m, MonadThrow m) => Tp.TopicRef -> m Tp.Topic
opGetTopic tr = Gh.getBy tr >>= getOrThrow (TopicNotFound tr)

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
testTopicPerm_ = void .*** testTopicPerm

-- | ensures the caller is joined to the topic, and then performs the same
-- action as testTopicPerm
testJoinTopicPerm :: (MonadThrow m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m Tp.Topic
testJoinTopicPerm caller tr modeP op = do
    topic <- opGetTopic tr
    mode <- opJoinTopic' caller tr topic
    unless (modeP (Tp.topicMode topic) mode) $ throwM (OperationDenied op)
    return topic

opSetTopicMode :: (MonadThrow m, MonadIO m, Functor m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Tp.TopicMode -> m Tp.TopicMode
opSetTopicMode caller tr newMode = do
    Gh.update [Tp.TopicModeField Gh.=. newMode] $ Tp.TopicCoord Gh.==. tr
    opCreateMessage_ caller tr $ Msg.MsgTopicModeChanged newMode
    return newMode

-- | unitary version of testJoinTopicPerm
testJoinTopicPerm_ :: (MonadThrow m, Functor m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> OpType -> m ()
testJoinTopicPerm_ = void .*** testJoinTopicPerm

opJoinTopic :: (MonadThrow m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> m Tm.MemberMode
opJoinTopic cr tr = opGetTopic tr >>= opJoinTopic' cr tr

opJoinTopic' :: (MonadThrow m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Tp.Topic -> m Tm.MemberMode
opJoinTopic' cr tr = opInsertMember cr tr . Tm.joinerMode . Tp.topicMode >=> either (return . Tm.memberMode) (sendJoinMessage . Tm.memberMode) 
    where sendJoinMessage mode = opCreateMessage cr tr (Msg.MsgUserJoined mode) >> return mode

-- | attempts to insert a member (and return it on the right side). if the member already exists, returns it on the left side.
opInsertMember :: (MonadThrow m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Tm.MemberMode -> m (Either Tm.Member Tm.Member)
opInsertMember ur tr mode =  Gh.getBy (Tm.TargetMemberKey tr ur) >>= maybe (insertMem newMember) (return . Left)
    where newMember = Tm.Member tr ur mode 
          insertMem m = Gh.insert m >> return (Right m)

opSetMemberMode :: (MonadThrow m, MonadIO m, Functor m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> Tm.MemberMode -> m Tm.MemberMode
opSetMemberMode callerRef tr targetUser newMode = do
    Gh.update [Tm.MemberModeField Gh.=. newMode] $ (Tm.MemberTopicField Gh.==. tr) Gh.&&. (Tm.MemberUserField Gh.==. targetUser)
    opCreateMessage_ callerRef tr $ Msg.MsgMemberModeChanged targetUser newMode
    return newMode

opLoadHandle :: (MonadThrow m, Gh.PersistBackend m) => Msg.MsgHandle -> m Msg.Message
opLoadHandle (Msg.MsgHandle tr mid sender k) = Gh.get k >>= maybe failure success
    where failure = throwM (LoadMessageFailed (Msg.MessageCoordKey tr mid))
          success = return . Msg.Message tr mid sender

opGetMessageHandle :: (MonadThrow m, Gh.PersistBackend m) => Msg.MessageRef -> m Msg.MsgHandle
opGetMessageHandle mr = Gh.getBy mr >>= getOrThrow (MessageNotFound mr)

--use this only in a transaction
opSaveMessage :: (MonadThrow m, Gh.PersistBackend m) => Msg.Message -> m Msg.Message
opSaveMessage (Msg.Message tr mid sender content) = opInsertMessage sender tr content (MessageIdInUse .* Msg.MessageCoordKey) mid

opCreateMessage :: (MonadThrow m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Msg.MsgContent -> m Msg.Message
opCreateMessage sender tr content = genRandom >>= opInsertMessage sender tr content ((. pure) . GenerateMessageIdFailed)

opCreateMessage_ :: (MonadThrow m, Functor m, MonadIO m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Msg.MsgContent -> m ()
opCreateMessage_ = void .** opCreateMessage

opInsertMessage :: (MonadThrow m, Gh.PersistBackend m) => Ur.UserRef -> Tp.TopicRef -> Msg.MsgContent -> (Tp.TopicRef -> MessageId -> OpError) -> MessageId -> m Msg.Message
opInsertMessage sender tr content err mid = do
    contentId <- Gh.insert content
    res <- Gh.insertByAll $ Msg.MsgHandle tr mid sender contentId
    when (isLeft res) $ throwM $ err tr mid
    return $ Msg.Message tr mid sender content

