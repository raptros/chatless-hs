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
import qualified Data.Map.Lazy as Map
import qualified Data.HashMap.Lazy as HM
import Control.Lens (at, (^.), (%~), (&), (<&>), (?~), view)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (MonadIO)

import Web.ReqRes.Types
import Web.ReqRes.Response

methodRoute :: MonadRespond m => Map.Map StdMethod (m ResponseReceived) -> m ResponseReceived
methodRoute dispatcher = getRequest <&> (parseMethod . requestMethod) >>= either (handleUnknownMethod supported) selectMethod
    where
    supported = Map.keys dispatcher
    selectMethod mth = fromMaybe (handleUnsupportedMethod supported mth) (dispatcher ^. at mth)

handleUnknownMethod :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived
handleUnknownMethod supported unknown = do
    handler <- getREH (view rehUnknownMethod)
    handler supported unknown

handleUnsupportedMethod :: MonadRespond m => [StdMethod] -> StdMethod -> m ResponseReceived
handleUnsupportedMethod supported unsupported = do
    handler <- getREH (view rehUnsupportedMethod)
    handler supported unsupported

defaultRequestErrorHandlers :: RequestErrorHandlers
defaultRequestErrorHandlers = RequestErrorHandlers defaultUnsupportedMethodHandler defaultUnknownMethodHandler defaultUnmatchedPathHandler

defaultUnsupportedMethodHandler allowed unsupported = respondMethodNotAllowed allowed (renderStdMethod unsupported)

defaultUnknownMethodHandler allowed unknown = respondMethodNotAllowed allowed unknown

defaultUnmatchedPathHandler known unknown = respond $ EmptyBody status404 []

-- wat
headTailSafeFold :: b -> (a -> [a] -> b) -> [a] -> b
headTailSafeFold def _ [] = def
headTailSafeFold _ f (a:as) = f a as

--segmentRoute :: MonadRespond m => (HM.HashMap T.Text (m ResponseReceived) -> m ResponseReceived
--segmentRoute map =
