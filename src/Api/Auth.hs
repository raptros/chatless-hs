{-# LANGUAGE OverloadedStrings #-}
module Api.Auth where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, withExceptT)
import Data.Monoid ((<>))
import Data.Aeson (object, (.=))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as Terr
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
callerAuth = runExceptT $ do
    header <- findHeader "x-chatless-test-uid" >>= throwEMaybe MissingHeader
    uid <- withExceptT (const BadEncoding) (decodeUtf8Trans header)
    serverId <- getServerId
    let userRef = UserCoordKey serverId (UserId uid) 
    runQuery (getBy userRef) >>= throwEMaybe (NoSuchUser userRef)

tryGetAuth :: (MonadRespond m, MonadChatless m) => Maybe User -> m (Maybe User)
tryGetAuth = maybe (either (const Nothing) Just <$> callerAuth) (return . Just)

throwEMaybe :: Monad m => e -> Maybe a -> ExceptT e m a
throwEMaybe e = maybe (throwE e) return

decodeUtf8Trans :: Monad m => BS.ByteString -> ExceptT Terr.UnicodeException m T.Text
decodeUtf8Trans = ExceptT . return . Tenc.decodeUtf8'

