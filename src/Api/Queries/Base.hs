{-|
Description: base types and utils for queries.

base types and utils for queries.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Queries.Base where

import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.Message as Msg
import Network.HTTP.Types.Status
import Data.Aeson
import Web.Respond
import qualified Database.Groundhog as Gh
import qualified Data.Text as T
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Except

-- | constraint synonym
type MonadControlIO m = (MonadBaseControl IO m, MonadIO m, MonadLogger m)

-- | why a query was denied
data QueryDenyReason =
    NotMember |
    ReadDenied

-- | text representations for query denials
queryDenyReasonText :: QueryDenyReason -> T.Text
queryDenyReasonText NotMember = "not_member"
queryDenyReasonText ReadDenied = "read_denied"

-- | reportable errors for query failures.
data QueryFailure =
    UserNotFound Ur.UserRef |
    TopicNotFound Tp.TopicRef |
    MemberNotFound Tp.TopicRef Ur.UserRef |
    QueryDenied QueryDenyReason |
    LoadMessageFailed Msg.MessageRef |
    MessageNotFound Msg.MessageRef

-- | the string not_found
textNotFound :: T.Text
textNotFound = "not_found"

textLoadMessageFailed :: T.Text
textLoadMessageFailed = "load_message_failed"

instance ReportableError QueryFailure where
    toErrorReport (UserNotFound ur) = errorReportWithDetails textNotFound $ object ["user" .= ur]
    toErrorReport (TopicNotFound tr) = errorReportWithDetails textNotFound $ object ["topic" .= tr]
    toErrorReport (MemberNotFound tr ur) = errorReportWithDetails textNotFound $ object ["topic" .= tr, "user" .= ur]
    toErrorReport (QueryDenied qdr) = simpleErrorReport (queryDenyReasonText qdr) 
    toErrorReport (LoadMessageFailed mr) = errorReportWithDetails textLoadMessageFailed $ object ["message" .= mr]
    toErrorReport (MessageNotFound mr) = errorReportWithDetails textNotFound $ object ["message" .= mr]

qfToResponseError :: QueryFailure -> ResponseError QueryFailure
qfToResponseError qf@(UserNotFound _)      = ResponseError notFound404 qf
qfToResponseError qf@(TopicNotFound _)     = ResponseError notFound404 qf
qfToResponseError qf@(MemberNotFound _ _)  = ResponseError notFound404 qf
qfToResponseError qf@(QueryDenied _)       = ResponseError forbidden403 qf
qfToResponseError qf@(LoadMessageFailed _) = ResponseError internalServerError500 qf
qfToResponseError qf@(MessageNotFound _)   = ResponseError notFound404 qf

instance ToResponse QueryFailure where
    toResponse = toResponse . qfToResponseError

getOrThrow :: MonadError e m => e -> Maybe a -> m a
getOrThrow err = maybe (throwError err) return

natToInt :: Natural -> Int
natToInt (Natural integer) = fromInteger integer

getCountDefault :: Maybe Natural -> Int
getCountDefault = maybe 1 natToInt

-- absolutely disgusting
instance MonadError QueryFailure m => MonadError QueryFailure (Gh.DbPersist conn m) where
    throwError = lift . throwError
    catchError act h = Gh.DbPersist $ catchError (Gh.unDbPersist act) (Gh.unDbPersist . h)

