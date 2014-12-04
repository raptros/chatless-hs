{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Api.Ops.Topic where

import Network.Wai
--import Network.HTTP.Types.Status
import Web.Respond
import qualified Database.Groundhog as Gh
import qualified Database.Groundhog.Core as Gh
import Control.Monad.Logger ()
--import Control.Monad.Except
--import Safe (headMay)
import Control.Applicative

import qualified Data.Text as T
--import Chatless.Model.ID
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
import qualified Chatless.Model.Message as Msg
import Api.Monad
import Api.Auth
import qualified Api.Queries as Q
import Control.Monad.Cont
import Control.Monad.Writer
import Control.Monad.Catch
import Data.Aeson
import Data.Typeable

-- * base
data TopicOp =
    SetBanner |
    SetInfo |
    SetTopicMode |
    SetMemberMode
    deriving (Eq, Show)

topicOpText :: TopicOp -> T.Text
topicOpText SetBanner = "set banner"
topicOpText SetInfo = "set info"
topicOpText SetTopicMode = "set topic mode"
topicOpText SetMemberMode = "set member mode"

data TopicOpFailure  = NotMember | NotPermitted TopicOp deriving (Eq, Show)

topicOpFailureReport :: TopicOpFailure -> Tp.TopicRef -> ErrorReport
topicOpFailureReport NotMember ref = errorReportWithDetails "not member" (object ["topic" .= ref])
topicOpFailureReport (NotPermitted op) ref = errorReportWithDetails "not permitted" (object ["topic" .= ref, "op" .= topicOpText op])

data TopicOpFailed = TopicOpFailed TopicOpFailure Tp.TopicRef deriving (Eq, Typeable, Show)

instance Exception TopicOpFailed
                   

-- * monad

{-
class (Gh.PersistBackend m, Functor m, MonadThrow m) => MonadTopicOp m where
    sentMessage :: Msg.MessageRef -> m ()
    failOp :: TopicOpFailure -> Tp.TopicRef -> m ()

newtype TopicOpT m a = TopicOpT {
    unTopicOpT :: WriterT [Msg.MessageRef] m a
} deriving (Functor, Applicative, Monad, MonadWriter [Msg.MessageRef])

instance (Gh.PersistBackend m, Functor m, MonadThrow m) => MonadTopicOp (TopicOpT m) where
    sentMessage m = tell [m]
    failOp f tr = throwM $ TopicOpFailed f tr

instance MonadTrans TopicOpT where
    lift = TopicOpT . lift

instance MonadThrow m => MonadThrow (TopicOpT m) where
    throwM = lift . throwM

runTopicOpT :: (MonadChatless m, MonadCatch m) => TopicOpT m a -> m (Either TopicOpFailed (a, [Msg.MessageRef]))
runTopicOpT act = try $ (runTransaction $ runWriterT $ unTopicOpT act) 
-}
