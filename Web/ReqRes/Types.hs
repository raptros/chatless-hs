{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Web.ReqRes.Types where

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
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl, StM)

class ToResponse a where
    toResponse :: a -> Response

instance ToResponse Response where
    toResponse = id

class HasRequest a where
    getRequest :: a -> Request

instance HasRequest Request where
    getRequest = id

type Responder = Response -> IO ResponseReceived

class HasResponder a where
    getResponder :: a -> Responder

data RequestErrorHandlers = RequestErrorHandlers {
    rehUnsupportedMethod :: (HasRequest a, HasResponder a, MonadIO m, MonadReader a m) => [StdMethod] -> StdMethod -> m ResponseReceived,
    rehUnknownMethod :: (HasRequest a, HasResponder a, MonadIO m, MonadReader a m) => [StdMethod] -> BS.ByteString -> m ResponseReceived
}

class (HasRequest a, HasResponder a) => HasRequestErrorHandlers a where
    getRequestErrorHandlers :: a -> RequestErrorHandlers
    modifyRequestErrorHandlers :: (RequestErrorHandlers -> RequestErrorHandlers) -> a -> a
    replaceRequestErrorHandlers :: RequestErrorHandlers -> a -> a
    replaceRequestErrorHandlers  = modifyRequestErrorHandlers . const

