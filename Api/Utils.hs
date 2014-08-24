{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Api.Utils where

import Data.Aeson ((.=), object, Value, ToJSON)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Status, badRequest400, forbidden403, notFound404, internalServerError500, notImplemented501)

import Yesod.Core (MonadHandler, TypedContent, toTypedContent)
import Yesod.Core.Handler (lookupHeader, sendResponseStatus, notAuthenticated)
import Yesod.Core.Json (returnJson)

import Model.ID (UserId(..))
import Model.User()
import Model.Topic()
import Operations (OpError(..), OpType(..))

reasonObject :: Text -> [(Text, Value)] -> Value
reasonObject r d = object $ ("reason" .= r) : d

returnErrorObject :: Monad m => Text -> [(Text, Value)] -> m TypedContent
returnErrorObject = ((return . toTypedContent) .) . reasonObject

notImplemented :: MonadHandler m => m Value
notImplemented = sendResponseStatus notImplemented501 ()

--todo this is of course stupid
extractUserId :: (MonadHandler m) => m UserId
extractUserId = lookupHeader "x-chatless-test-uid" >>= maybe notAuthenticated (return . UserId . decodeUtf8)

respondOpResult :: (MonadHandler m, ToJSON a) => Either OpError a -> m Value
respondOpResult = either respondOpError returnJson

respondOp :: (MonadHandler m) => Either OpError a -> m a
respondOp = either respondOpError return

objection :: MonadHandler m => Status -> Text -> [(Text, Value)] -> m a
objection s t v = sendResponseStatus s $ reasonObject t v

respondOpError :: MonadHandler m => OpError -> m a
respondOpError (MeNotFound ur) = objection internalServerError500 "not_found" ["me" .= ur]
respondOpError (TopicNotFound tr) = objection notFound404 "not_found" ["topic" .= tr]
respondOpError (UserNotFound ur) = objection notFound404 "not_found" ["user" .= ur]
respondOpError (MemberNotFound tr ur) = objection notFound404 "not_found" ["topic" .= tr, "user" .= ur]
respondOpError (MessageNotFound mr) = objection notFound404 "not_found" ["message" .= mr]
respondOpError (OperationDenied ot) = objection forbidden403 "forbidden" ["operation" .= opTypeName ot]
respondOpError (IdInUse tr) = objection badRequest400 "id_in_use" ["topic" .= tr]
respondOpError (GenerateIdFailed ur ids) = objection internalServerError500 "generate_id_failed" ["user" .= ur, "tried" .= ids]
respondOpError (GenerateMessageIdFailed tr ids) = objection internalServerError500 "generate_id_failed" ["topic" .= tr, "tried" .= ids]
respondOpError (MessageIdInUse mr) = objection badRequest400 "id_in_use" ["message" .= mr] --basically not something we should see.
respondOpError (LoadMessageFailed mr) = objection internalServerError500 "load_message_failed" ["message" .= mr]
respondOpError (AlreadyMember tr ur) = objection badRequest400 "already_member" ["topic" .= tr, "user" .= ur]
respondOpError (NotImplemented ot) = objection notImplemented501 "not_implemented" ["operation" .= opTypeName ot]

opTypeName :: OpType -> Text
opTypeName ReadTopic = "read_topic"
opTypeName SetMemberMode = "set_member_mode"
opTypeName SetTopicMode = "set_topic_mode"
opTypeName SendMessage = "send_message"
opTypeName SetBanner = "set_banner"
opTypeName InviteUser = "invite_user"
opTypeName GetRemoteUser = "get_remote_user"
