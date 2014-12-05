{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Api.Ops.Topic where

--import Network.HTTP.Types.Status
import Web.Respond
import qualified Database.Groundhog as Gh
import Control.Monad.Logger ()
--import Control.Monad.Except
--import Safe (headMay)
import Api.Queries ()

import qualified Data.Text as T
--import Chatless.Model.ID
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
import qualified Chatless.Model.Message as Msg
import Api.Monad
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
                   
opGetEffectiveMode :: (MonadThrow m, Functor m, Gh.PersistBackend m) => Tp.Topic -> Ur.User -> m (Tm.MemberMode)
opGetEffectiveMode topic user
    | Tp.isUserCreator user topic = return Tm.modeCreator
    | otherwise = Gh.getBy (Tm.TargetMemberKey tRef uRef) >>= maybe (throwM $ TopicOpFailed NotMember tRef) (return . Tm.memberMode)
    where
    tRef = Tp.getRefFromTopic topic
    uRef = Ur.getRefFromUser user

changeBanner :: (MonadRespond m, MonadChatless m, MonadCatch m) => Ur.User -> Tp.Topic -> T.Text -> m (Either TopicOpFailed [Msg.MessageRef])
changeBanner caller topic newBanner = try $ runTransaction $ execWriterT $ do
    member <- lift $ opGetEffectiveMode topic caller 
    let tRef = Tp.getRefFromTopic topic
    unless (Tm.mmSetBanner member) $ throwM $ TopicOpFailed (NotPermitted SetBanner) tRef

-- absolutely disgusting
instance MonadThrow m => MonadThrow (Gh.DbPersist conn m) where
    throwM = lift . throwM
