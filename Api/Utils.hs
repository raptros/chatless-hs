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

