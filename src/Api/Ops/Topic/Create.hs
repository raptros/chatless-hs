{- |

operation etc to create a topic
-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Ops.Topic.Create where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Writer
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Maybe
import Control.Exception.Lifted (try)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.Random (randomIO)

import qualified Database.Groundhog as Gh

import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
import qualified Chatless.Model.Message as Msg
import Chatless.Model.ID

import Api.Monad
import Api.Ops.Base
import Api.Ops.Topic.Base
import Api.Ops.Topic.Parts

evalWriterT :: Monad m => WriterT w m a -> m a
evalWriterT = liftM fst . runWriterT

createTopic :: (MonadChatless m, MonadBaseControl IO m) => Ur.User -> Tp.TopicCreate -> m TopicCreateResult
createTopic caller create = try . evalWriterT . runTransaction $ do
    topic <- opCreateTopic caller create
    void $ opAddMember CreateTopic (const Tm.modeCreator) Msg.MsgUserJoined caller topic (Ur.getRefFromUser caller)
    return $ Tp.getRefFromTopic topic

topicCreateMaxAttempts :: Int
topicCreateMaxAttempts = 3

opCreateTopic :: (Gh.PersistBackend m, MonadBase IO m) => Ur.User -> Tp.TopicCreate -> m Tp.Topic
opCreateTopic caller = maybe <$> opGenerateIdForTopic caller <*> opSaveTopicToId caller <*> Tp.createId

opGenerateIdForTopic :: (Gh.PersistBackend m, MonadBase IO m) => Ur.User -> Tp.TopicCreate -> m Tp.Topic
opGenerateIdForTopic caller create = runAttempts topicCreateMaxAttempts >>= throwOrReturn <$> onFail . snd <*> fst
    where
    -- note that the topicRef used with GenerateTopicIdFailed is the about topic of the caller.
    onFail tried = TopicOpFailed CreateTopic (GenerateTopicIdFailed tried) (Tp.userAboutTopicRef caller)
    runAttempts count = runWriterT . runMaybeT . retryNTimes count $ do
        tid <- liftBase randomIO
        tell [tid]
        let topic = Tp.initializeTopic (Ur.getRefFromUser caller) tid create
        MaybeT $ lift $ opWriteTopic topic

opSaveTopicToId :: (Gh.PersistBackend m, MonadBase IO m, Functor m) => Ur.User -> Tp.TopicCreate -> TopicId -> m Tp.Topic
opSaveTopicToId caller create tid = opWriteTopic topic >>= throwOrReturn onFail 
    where
    topic = Tp.initializeTopic (Ur.getRefFromUser caller) tid create
    onFail = TopicOpFailed CreateTopic TopicIdInUse (Tp.userTopicRef caller tid)

opWriteTopic :: (Gh.PersistBackend m, Functor m) => Tp.Topic -> m (Maybe Tp.Topic)
opWriteTopic topic = whenRight topic <$> Gh.insertByAll topic
