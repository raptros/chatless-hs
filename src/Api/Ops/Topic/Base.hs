{- |

types and functions for building topic ops and describing topic op results
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Ops.Topic.Base where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Typeable
import qualified Data.Text as T

import Control.Monad.Cont
import Control.Monad.Writer
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted (try, Exception)
import Control.Monad.Logger (NoLoggingT)

import qualified Database.Groundhog as Gh
import Network.HTTP.Types.Status
import Network.Wai
import Web.Respond

import Chatless.Model.ID
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
import qualified Chatless.Model.Message as Msg

import Api.Config
import Api.Monad

-- * results

-- | the operation types
data TopicOp =
    SetBanner |
    SetInfo |
    SetTopicMode |
    SetMemberMode |
    SendMessage |
    SendInvite |
    JoinTopic |
    CreateTopic
    deriving (Eq, Show)

-- | strings representing the operations
topicOpText :: TopicOp -> T.Text
topicOpText SetBanner = "set banner"
topicOpText SetInfo = "set info"
topicOpText SetTopicMode = "set topic mode"
topicOpText SetMemberMode = "set member mode"
topicOpText SendMessage = "send message"
topicOpText SendInvite = "send invite"
topicOpText JoinTopic = "join topic"
topicOpText CreateTopic = "create topic"

-- | produce a predicate determining if a user is allowed to perform the
-- specified operation
topicOpAllowed :: TopicOp -> Tp.TopicMode -> Tm.MemberMode -> Bool
topicOpAllowed SetBanner = const Tm.mmSetBanner
topicOpAllowed SetInfo = const Tm.mmSetInfo
topicOpAllowed SetTopicMode = const Tm.mmSetMode
topicOpAllowed SetMemberMode = const Tm.mmSetMember
topicOpAllowed SendMessage = Tm.canSend
topicOpAllowed SendInvite = const Tm.mmInvite
topicOpAllowed JoinTopic = const (const True)
topicOpAllowed CreateTopic = const (const True)

-- | various ways a topic operation can fail
data TopicOpFailureReason  = 
    CallerNotMember | 
    TargetNotMember (Ur.UserRef) |
    NotPermitted |
    MsgIdInUse MessageId |
    MsgIdGenFailed [MessageId] |
    TargetAlreadyMember (Ur.UserRef) |
    NoSuchUser (Ur.UserRef) |
    InviteTopicMissing (Tp.TopicRef) |
    GenerateTopicIdFailed [TopicId] |
    TopicIdInUse
    deriving (Eq, Show)

-- | convert the failure with the topic ref into an error report
tofrPrepare :: TopicOpFailureReason -> (T.Text, [Pair])
tofrPrepare CallerNotMember              = ("not member", [])
tofrPrepare (TargetNotMember uref)       = ("not member", ["user" .= uref])
tofrPrepare NotPermitted                 = ("not permitted", [])
tofrPrepare (MsgIdInUse mid)             = ("message id in use", ["mid" .= mid])
tofrPrepare (MsgIdGenFailed mids)        = ("message id generation failed", ["tried" .= mids])
tofrPrepare (TargetAlreadyMember uref)   = ("already member", ["user" .= uref])
tofrPrepare (NoSuchUser uref)            = ("no such user", ["user" .= uref])
tofrPrepare (InviteTopicMissing tref)    = ("invite topic missing", ["targetRef" .= tref])
tofrPrepare (GenerateTopicIdFailed tids) = ("topic id generation failed", ["tried" .= tids])
tofrPrepare TopicIdInUse                 = ("topic id in use", [])


-- | status codes appropriate for each failure type
topicOpFailureStatus :: TopicOpFailureReason-> Status
topicOpFailureStatus CallerNotMember           = forbidden403
topicOpFailureStatus (TargetNotMember _)       = notFound404
topicOpFailureStatus NotPermitted              = forbidden403
topicOpFailureStatus (MsgIdInUse _)            = badRequest400
topicOpFailureStatus (MsgIdGenFailed _)        = internalServerError500
topicOpFailureStatus (TargetAlreadyMember _)   = badRequest400
topicOpFailureStatus (NoSuchUser _)            = notFound404
topicOpFailureStatus (InviteTopicMissing _)    = internalServerError500
topicOpFailureStatus (GenerateTopicIdFailed _) = internalServerError500
topicOpFailureStatus TopicIdInUse              = badRequest400

-- | wrapper around a failure type and a topic ref; it can be thrown and it
-- can be reported as a response body
data TopicOpFailed = TopicOpFailed {
    tofOperation :: TopicOp,
    tofReason :: TopicOpFailureReason,
    tofTopicRef :: Tp.TopicRef
} deriving (Eq, Typeable, Show)

instance Exception TopicOpFailed

topicOpFailedErrorReport :: TopicOpFailed -> ErrorReport
topicOpFailedErrorReport (TopicOpFailed op fReason tref) = errorReportWithDetails reasonText $ object (moreDetails ++ details)
    where
    details = ["topic" .= tref, "op" .= topicOpText op]
    (reasonText, moreDetails) = tofrPrepare fReason

instance ReportableError TopicOpFailed where
    reportError = reportAsErrorReport topicOpFailedErrorReport

topicOpFailedStatus :: TopicOpFailed -> Status
topicOpFailedStatus = topicOpFailureStatus . tofReason

respondTopicOpFailed :: MonadRespond m => TopicOpFailed -> m ResponseReceived
respondTopicOpFailed failed = respondReportError (topicOpFailedStatus failed) [] failed

type TopicOpResult = Either TopicOpFailed [Msg.MessageRef]

respondMessagesCreated :: MonadRespond m => [Msg.MessageRef] -> m ResponseReceived
respondMessagesCreated msgs
    | null msgs = respondEmptyBody noContent204 []
    | otherwise = respondOk $ Json msgs 

respondTopicOpResult :: MonadRespond m => TopicOpResult -> m ResponseReceived
respondTopicOpResult = either respondTopicOpFailed respondMessagesCreated

type TopicCreateResult = Either TopicOpFailed Tp.TopicRef

respondTopicCreateResult :: MonadRespond m => TopicCreateResult -> m ResponseReceived
respondTopicCreateResult = either respondTopicOpFailed $ respondWith created201 [] . Json 

-- * construction

-- | topic operations need to record all the messages they will insert
type TopicOperationT m a = Gh.DbPersist CLDb (NoLoggingT (WriterT [Msg.MessageRef] m)) a

type MonadMessages m = (Functor m, MonadBase IO m, MonadIO m, Gh.PersistBackend m, MonadWriter [Msg.MessageRef] m)

-- | runs the topic operaton and catches any TopicOpFailed exceptions
tryTopicOp :: (MonadChatless m, MonadBaseControl IO m) => TopicOperationT m () -> m TopicOpResult
tryTopicOp = try . execWriterT . runTransaction
