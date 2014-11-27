{-|
Description: api client for chatless

api client library for chatless.
-}
{-# LANGUAGE TemplateHaskell #-}

module Chatless.Client where

import Control.Applicative
import Network.HTTP.Client
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Chatless.Model.User
import Chatless.Model.Topic
import Control.Monad.IO.Class
import Data.Default as Def

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid

data ApiBase = ApiBase {
    apiHost :: BS.ByteString,
    apiPort :: Int
} deriving (Eq, Show)

data Session = Session {
    sessionUser :: BS.ByteString,
    sessionApi :: ApiBase,
    sessionManager :: Manager
}

data ErrorReport {
    reason :: T.Text,
    message :: Maybe T.Text,
    details :: Maybe Value
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''ErrorReport)

mkAbsPath :: [BS.ByteString] -> BS.ByteString
mkAbsPath pth = mconcat $ fmap ("/" <>) pth

mkCLRequest :: MonadIO m => Session -> [BS.ByteString] -> StdMethod -> RequestHeaders -> RequestBody -> m (Response BSL.ByteString)
mkCLRequest ses pth mth hdr bdy = liftIO $ httpLbs req (sessionManager ses)
    where
    req :: Request
    req = Def.def { 
        method = renderStdMethod mth,
        host = apiHost . sessionApi $ ses,
        port = apiPort . sessionApi $ ses,
        path = mkAbsPath pth,
        requestHeaders = ("x-chatless-test-uid", sessionUser ses):hdr,
        requestBody = bdy
    }

mkCLQuery :: MonadIO m => Session -> [BS.ByteString] -> m (Response BSL.ByteString)
mkCLQuery ses pth =  mkCLRequest ses pth GET [] (RequestBodyBS "")

queryMe :: (Functor m, MonadIO m) => Session -> m (Response (Either String User))
queryMe ses = fmap (eitherDecode' <$>) $ mkCLQuery ses ["me"]



