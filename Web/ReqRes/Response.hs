{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

respond :: (HasResponder a, MonadIO m, MonadReader a m, ToResponse v) => v -> m ResponseReceived
respond v = do
    r <- reader getResponder
    liftIO . r . toResponse $ v


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

responseJson :: ToJSON a => Status -> ResponseHeaders -> a -> Response
responseJson s hs a = responseLBS s ((hContentType, "application/json") : hs) (encode a)

respondMethodNotAllowed :: (HasResponder a, MonadIO m, MonadReader a m) => [StdMethod] -> Method -> m ResponseReceived
respondMethodNotAllowed allowed tried = respond $ EmptyJson methodNotAllowed405 [("Allowed", allowedStr)]
    where allowedStr = BS.intercalate ", " (renderStdMethod <$> allowed)

responseMethodNotAllowedStd :: ToJSON a => [StdMethod] -> a -> Response
responseMethodNotAllowedStd = responseMethodNotAllowed . fmap renderStdMethod

responseMethodNotAllowed :: ToJSON a => [Method] -> a -> Response
responseMethodNotAllowed allowed = responseJson methodNotAllowed405 [("Allowed", allowedStr)]
    where allowedStr = BS.intercalate ", " allowed

