{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Ops where

import Network.Wai
import Network.HTTP.Types.Status
import Web.Respond
import qualified Database.Groundhog as Gh
import Control.Monad.Logger ()
import Control.Monad.Except
import Safe (headMay)

import qualified Data.Text as T
import Chatless.Model.ID
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.Message as Msg
import Api.Monad
import Api.Auth

-- * base
data TopicOp =
    SetBanner |
    SetInfo |
    SetTopicMode |
    SetMemberMode
    deriving (Eq, Show)

data TopicOpDenyReason  = NotMember | NotPermitted deriving (Eq, Show)

opDeniedMessage :: TopicOp -> T.Text
opDeniedMessage SetBanner = "set banner denied"
opDeniedMessage SetInfo = "set info denied"
opDeniedMessage SetTopicMode = "set topic mode denied"
opDeniedMessage SetMemberMode = "set member mode denied"

data OpFailure =
    TopicOpDenied TopicOpDenyReason TopicOp TopicRef |
    deriving (Eq, Show)

opFailErrorReport :: OpFailure -> ErrorReport
opFailErrorReport (TopicOpDenied op ref) = fullErrorReport "operation denied" (opDeniedMessage op) (object ["topic" .= ref])

instance ReportableError OpFailure where
    reportError = (. opFailErrorReport) . reportError


-- * topic ops
topicOpGuard :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> TopicOp -> (Tp.TopicMode -> Tm.MemberMode -> Bool) -> m ResponseReceived -> m ResponseReceived
topicOpGuard maybeCaller topicData op pred inner = callerReauth maybeCaller $ \caller ->
    withMemberAuth topicData caller (
    | Tp.membershipRequired topicData = reauth $ \caller -> 
        withMemberAuth topicData caller (QueryDenied NotMember) $ \memberMode ->
            if Tm.mmRead memberMode || Tp.isUserCreator caller topicData then inner else handleDenied $ QueryDenied ReadDenied
    | Tp.authRequired topicData = reauth (const inner)
    | otherwise = inner
    where
    reauth = callerReauth maybeCaller 

-- ** topic fields ops

