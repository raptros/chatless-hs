{- |

internal ops for creating messages in a topic
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Ops.Topic.Message where

import System.Random (randomIO)
import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Base (MonadBase, liftBase)

import qualified Database.Groundhog as Gh

import Chatless.Model.ID
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.Message as Msg

import Api.Ops.Base
import Api.Ops.Topic.Base


opWriteMessage :: MonadMessages m => TopicOp -> Ur.User -> Tp.Topic -> Msg.MsgContent -> m ()
opWriteMessage op sender topic content = do
    msg <- opAttemptMsgCreate op sender topic content
    tell [Msg.getRefFromMessage msg]

-- | how many times 'opAttemptMsgCreate' should attempt to generate an id
-- and save the message
msgCreateMaxAttempts :: Int
msgCreateMaxAttempts = 3

opAttemptMsgCreate :: (Functor m, MonadBase IO m, Gh.PersistBackend m) => TopicOp -> Ur.User -> Tp.Topic -> Msg.MsgContent -> m Msg.Message
opAttemptMsgCreate op sender topic content = runAttempts msgCreateMaxAttempts >>= throwOrReturn <$> onFail . snd <*> fst
    where
    onFail tried = TopicOpFailed op (MsgIdGenFailed tried) (Tp.getRefFromTopic topic)
    runAttempts count = runWriterT . runMaybeT . retryNTimes count $ do
        mid <- liftBase randomIO
        tell [mid]
        MaybeT $ lift $ opStoreMessage sender topic content mid

opStoreMessage :: Gh.PersistBackend m => Ur.User -> Tp.Topic -> Msg.MsgContent -> MessageId -> m (Maybe Msg.Message)
opStoreMessage sender topic content mid = do
    contentId <- Gh.insert content
    res <- Gh.insertByAll $ Msg.MsgHandle topicRef mid senderRef contentId
    return $ whenRight (Msg.Message topicRef mid senderRef content) res
    where
    topicRef = Tp.getRefFromTopic topic
    senderRef = Ur.getRefFromUser sender
