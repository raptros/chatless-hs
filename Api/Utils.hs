{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Api.Utils where

import Yesod.Core
import Data.Aeson
import Data.Text
import Network.HTTP.Types
import Data.Text.Encoding (decodeUtf8)
import Model.ID
import Model.User
import Model.Topic
import Control.Monad.Except
import Operations

reasonObject :: Text -> [(Text, Value)] -> Value
reasonObject r d = object $ ("reason" .= r) : d

returnErrorObject :: Monad m => Text -> [(Text, Value)] -> m TypedContent
returnErrorObject = ((return . toTypedContent) .) . reasonObject

notImplemented :: MonadHandler m => m Value
notImplemented = sendResponseStatus status501 ()

userNotFound :: MonadHandler m => ServerId -> UserId -> m a
userNotFound sid uid = sendResponseStatus notFound404 $ reasonObject "not_found" ["coordinate" .= object ["server" .= sid, "user" .= uid]]

topicNotFound :: MonadHandler m => TopicRef -> m a
topicNotFound tr = sendResponseStatus notFound404 $ reasonObject "not_found" ["coordinate" .= tr]

--todo this is of course stupid
extractUserId :: (MonadHandler m) => m UserId
extractUserId = lookupHeader "x-chatless-test-uid" >>= maybe notAuthenticated (return . UserId . decodeUtf8)

meNotPresent :: MonadHandler m => UserRef -> m a
meNotPresent ref= sendResponseStatus status500 $ reasonObject "me_not_present" ["coordinate" .= ref]

respondOpResult :: (MonadHandler m, ToJSON a) => Either OpError a -> m Value
respondOpResult = either respondOpError returnJson

respondOpError :: MonadHandler m => OpError -> m a
respondOpError (MeNotFound ur) = sendResponseStatus internalServerError500 $ reasonObject "me_not_found" ["coordinate" .= ur]
respondOpError (TopicNotFound tr) = sendResponseStatus notFound404 $ reasonObject "topic_not_found" ["coordinate" .= tr]
respondOpError (UserNotFound ur) = sendResponseStatus notFound404 $ reasonObject "user_not_found" ["coordinate" .= ur]
respondOpError (MemberNotFound tr ur) = sendResponseStatus notFound404 $ reasonObject "member_not_found" ["topic" .= tr, "user" .= ur]
respondOpError (OperationDenied ot) = sendResponseStatus forbidden403 $ reasonObject "forbidden" ["operation" .= opTypeName ot]

opTypeName :: OpType -> Text
opTypeName ReadTopic = "read_topic"
opTypeName SetMemberMode = "set_member_mode"
               {-
    MeNotFound UserRef |
    TopicNotFound TopicRef |
    UserNotFound UserRef |
    MemberNotFound TopicRef UserRef |
    OperationDenied OpType |
    IdInUse TopicRef |
GenerateIdFailed UserRef [TopicId]-}
