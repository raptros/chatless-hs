{-# LANGUAGE OverloadedStrings #-}
module Api.Utils where

import Yesod.Core
import Data.Aeson
import Data.Text
import Network.HTTP.Types
import Data.Text.Encoding (decodeUtf8)
import Model.ID

reasonObject :: Text -> [(Text, Value)] -> Value
reasonObject r d = object $ ("reason" .= r) : d

returnErrorObject :: Monad m => Text -> [(Text, Value)] -> m TypedContent
returnErrorObject = ((return . toTypedContent) .) . reasonObject

notImplemented :: MonadHandler m => m Value
notImplemented = sendResponseStatus status501 ()


--todo this is of course stupid
extractUserId :: (MonadHandler m) => m UserId
extractUserId = lookupHeader "x-chatless-test-uid" >>= maybe notAuthenticated (return . UserId . decodeUtf8)

