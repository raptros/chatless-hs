{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.ReqRes.Request where

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

import Web.ReqRes.Types
import Web.ReqRes.Response

methodRoute :: (HasRequestErrorHandlers t, MonadIO m, MonadReader t m) => Map.Map StdMethod (m ResponseReceived) -> m ResponseReceived
methodRoute dispatcher = reader (requestMethod . getRequest) >>= either (handleUnknownMethod supported) selectMethod . parseMethod
    where
    supported = Map.keys dispatcher
    selectMethod mth = fromMaybe (handleUnsupportedMethod supported mth) (dispatcher ^. at mth)

handleUnknownMethod :: (HasRequestErrorHandlers t, MonadIO m, MonadReader t m) => [StdMethod] -> Method -> m ResponseReceived
handleUnknownMethod supported unknown = do
    handler <- reader (rehUnknownMethod . getRequestErrorHandlers)
    handler supported unknown

handleUnsupportedMethod :: (HasRequestErrorHandlers t, MonadIO m, MonadReader t m) => [StdMethod] -> StdMethod -> m ResponseReceived
handleUnsupportedMethod supported unsupported = do
    handler <- reader (rehUnsupportedMethod . getRequestErrorHandlers)
    handler supported unsupported

localModifyREHs :: (HasRequestErrorHandlers t, MonadReader t m) => (RequestErrorHandlers -> RequestErrorHandlers) -> m a -> m a
localModifyREHs = local . modifyRequestErrorHandlers


defaultRequestErrorHandlers :: RequestErrorHandlers
defaultRequestErrorHandlers = RequestErrorHandlers {
    rehUnsupportedMethod = defaultUnsupportedMethodHandler,
    rehUnknownMethod = defaultUnknownMethodHandler
}

defaultUnsupportedMethodHandler allowed unsupported = respondMethodNotAllowed allowed (renderStdMethod unsupported)

defaultUnknownMethodHandler allowed unknown = respondMethodNotAllowed allowed unknown

-- wat
headTailSafeFold :: b -> (a -> [a] -> b) -> [a] -> b
headTailSafeFold def _ [] = def
headTailSafeFold _ f (a:as) = f a as


