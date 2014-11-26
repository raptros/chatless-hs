{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Auth where

import Control.Applicative ((<$>))
import Control.Monad.Except
import Data.Monoid ((<>))
import Data.Aeson (object, (.=))
import Network.Wai (ResponseReceived)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Tenc
import Database.Groundhog

import Web.Respond

import Model.ID
import Model.User

import Api.Monad

data AuthFailure =
    MissingHeader |
    BadEncoding |
    MalformedHeader (T.Text) |
    NoSuchUser (UserRef)
    deriving (Eq, Show)

instance ReportableError AuthFailure where
    toErrorReport MissingHeader = ErrorReport "missing_header" Nothing Nothing
    toErrorReport BadEncoding = ErrorReport "bad_encoding" (Just "request must be UTF8") Nothing
    toErrorReport (MalformedHeader malformed) = ErrorReport "malformed_header" (Just $  "bad header: " <> malformed) Nothing
    toErrorReport (NoSuchUser user) = ErrorReport "no_such_user" Nothing (Just $ object ["user" .= user])

callerAuth :: (MonadRespond m, MonadChatless m) => m (Either AuthFailure User)
callerAuth = runExceptT callerAuthInner

callerAuthInner :: (MonadRespond m, MonadChatless m, MonadError AuthFailure m) => m User
callerAuthInner = do
    header <- findHeader "x-chatless-test-uid" >>= throwMaybe MissingHeader
    uid <- either (const $ throwError BadEncoding) return $ Tenc.decodeUtf8' header
    serverId <- getServerId
    let userRef = UserCoordKey serverId (UserId uid) 
    runQuery (getBy userRef) >>= throwMaybe (NoSuchUser userRef)

tryGetAuth :: (MonadRespond m, MonadChatless m) => Maybe User -> m (Maybe User)
tryGetAuth = maybe (either (const Nothing) Just <$> callerAuth) (return . Just)

throwMaybe :: (MonadError e m) => e -> Maybe a -> m a
throwMaybe e = maybe (throwError e) return

callerReauth :: (MonadRespond m, MonadChatless m) => Maybe User -> (User -> m ResponseReceived) -> m ResponseReceived
callerReauth maybeCaller = reauthenticate maybeCaller callerAuth
