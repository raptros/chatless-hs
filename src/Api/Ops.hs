{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Ops where

import Network.Wai
--import Network.HTTP.Types.Status
import Web.Respond
import qualified Database.Groundhog as Gh
import qualified Database.Groundhog.Core as Gh
import Control.Monad.Logger ()
--import Control.Monad.Except
--import Safe (headMay)

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
import Data.Aeson
import Api.Ops.Topic

-- * base
data OpFailure =
    TopicOpFailed TopicOpFailure Tp.TopicRef 
    deriving (Eq, Show)

opFailErrorReport :: OpFailure -> ErrorReport
opFailErrorReport (TopicOpFailed f ref) = topicOpFailureReport f ref

instance ReportableError OpFailure where
    reportError = (. opFailErrorReport) . reportError

-- * topic ops
topicOpGuard :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> TopicOp -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> m ResponseReceived -> m ResponseReceived
topicOpGuard maybeCaller topicData op checkModes inner = (`runCont` id) $ do
    caller <- cont $ callerReauth maybeCaller 
    let tr = Tp.getRefFromTopic topicData
    memberMode <- cont $ Q.withMemberAuth topicData caller (TopicOpDenied NotMember tr) 
    return $ authorizeBool (TopicOpDenied (TONotPermitted op) tr) (checkModes (Tp.topicMode topicData) memberMode) inner

-- ** topic fields ops

opChangeBanner :: (PersistBackend m,
