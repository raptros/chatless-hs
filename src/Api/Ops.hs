{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Ops where

import Network.Wai
--import Network.HTTP.Types.Status
import Web.Respond
--import qualified Database.Groundhog as Gh
import Control.Monad.Logger ()
--import Control.Monad.Except
--import Safe (headMay)

import qualified Data.Text as T
--import Chatless.Model.ID
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
--import qualified Chatless.Model.Message as Msg
import Api.Monad
import Api.Auth
import qualified Api.Queries as Q
import Control.Monad.Cont
import Data.Aeson

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

data TopicOpDenial  = NotMember | TONotPermitted TopicOp deriving (Eq, Show)

topicOpDenialReport :: TopicOpDenial -> Tp.TopicRef -> ErrorReport
topicOpDenialReport NotMember ref = errorReportWithDetails "not member" (object ["topic" .= ref])
topicOpDenialReport (TONotPermitted op) ref = errorReportWithDetails "not permitted" (object ["topic" .= ref, "op" .= topicOpText op])

data OpFailure =
    TopicOpDenied TopicOpDenial Tp.TopicRef 
    deriving (Eq, Show)

opFailErrorReport :: OpFailure -> ErrorReport
opFailErrorReport (TopicOpDenied denial ref) = topicOpDenialReport denial ref

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
setBanner :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> T.Text -> m ResponseReceived
setBanner maybeCaller topicData banner = topicOpGuard maybeCaller topicData $ 
{-
setBanner :: (MonadRespond m, MonadChatless m) => Ur.UserRef -> Tp.TopicRef -> T.Text -> m (Either OpError Msg.Message)
setBanner caller tr banner = runOp $ do
    T.testTopicPerm_ caller tr (const Tm.mmSetBanner) SetBanner
    Gh.update [Tp.TopicBannerField Gh.=. banner] $ Tp.TopicCoord Gh.==. tr
    T.opCreateMessage caller tr $ Msg.MsgBannerChanged banner
-}
