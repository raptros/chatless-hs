{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Web.ReqRes.Response where

import Control.Applicative ((<$>))
import Model.ID
import Network.Wai
import Data.Aeson
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map.Lazy as Map
import Control.Lens (at, (^.), (&), (?~))
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM)

import Web.ReqRes.Types

data AsJson a =
        AddHeaders Status ResponseHeaders a |
        DefaultHeaders Status a |
        OkJson a

instance ToJSON a => ToResponse (AsJson a) where
    toResponse (AddHeaders status hdrs a) = responseJson status hdrs a
    toResponse (DefaultHeaders status a) = responseJson status [] a
    toResponse (OkJson a) = responseJson ok200 [] a

data EmptyJson = EmptyJson Status ResponseHeaders

instance ToResponse EmptyJson where
    toResponse (EmptyJson status hdrs) = responseJson status hdrs (object [])

data EmptyBody = EmptyBody Status ResponseHeaders

instance ToResponse EmptyBody where
    toResponse (EmptyBody status hdrs) = responseLBS status (headerCTJson : (hContentLength, "0") : hdrs) ""

contentTypeJson :: BS.ByteString
contentTypeJson = "application/json"

mkContentType :: BS.ByteString -> Header
mkContentType = (hContentType, )

headerCTJson :: Header
headerCTJson = mkContentType contentTypeJson

responseJson :: ToJSON a => Status -> ResponseHeaders -> a -> Response
responseJson s hs a = responseLBS s (headerCTJson : hs) (encode a)

respondMethodNotAllowed :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived
respondMethodNotAllowed allowed tried = respond $ EmptyBody methodNotAllowed405 [("Allowed", allowedStr)]
    where allowedStr = BS.intercalate ", " (renderStdMethod <$> allowed)

responseMethodNotAllowedStd :: ToJSON a => [StdMethod] -> a -> Response
responseMethodNotAllowedStd = responseMethodNotAllowed . fmap renderStdMethod

responseMethodNotAllowed :: ToJSON a => [Method] -> a -> Response
responseMethodNotAllowed allowed = responseJson methodNotAllowed405 [("Allowed", allowedStr)]
    where allowedStr = BS.intercalate ", " allowed

