{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.ReqRes.Request where

import Control.Applicative
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
import Safe (headMay)

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

defaultUnsupportedMethodHandler :: MonadRespond m => [StdMethod] -> StdMethod -> m ResponseReceived
defaultUnsupportedMethodHandler allowed unsupported = respondMethodNotAllowed allowed (renderStdMethod unsupported)

defaultUnknownMethodHandler :: MonadRespond m => [StdMethod] -> BS.ByteString -> m ResponseReceived
defaultUnknownMethodHandler allowed unknown = respondMethodNotAllowed allowed unknown

defaultUnmatchedPathHandler :: MonadRespond m => m ResponseReceived
defaultUnmatchedPathHandler = respond $ EmptyBody status404 []

-- wat
headTailSafeFold :: b -> (a -> [a] -> b) -> [a] -> b
headTailSafeFold def _ [] = def
headTailSafeFold _ f (a:as) = f a as

{-
segmentRoute :: MonadRespond m => m ResponseReceived -> (HM.HashMap T.Text (m ResponseReceived)) -> m ResponseReceived
segmentRoute onRoot map = getNextSegment >>= maybe onRoot pickNext
    where
    pickNext seg = maybe handleUnmatchedPath withNextSegmentConsumed (HM.lookup map seg)
-}

handleUnmatchedPath :: MonadRespond m => m ResponseReceived
handleUnmatchedPath = do
    handler <- getREH (view rehUnmatchedPath)
    handler

matchPath :: MonadRespond m => PathMatcher (m ResponseReceived) -> m ResponseReceived
matchPath pm = getUnconsumedPath >>= (fromMaybe handleUnmatchedPath . runPathMatcher pm)

mayWhen :: a -> Bool -> Maybe a
mayWhen v True = Just v
mayWhen _ False = Nothing

rootMatcher :: a -> PathMatcher a
rootMatcher a = PathMatcher $ mayWhen a . null

segMatcher :: MonadRespond m => T.Text -> m a -> PathMatcher (m a)
segMatcher segWant act = PathMatcher $ \rest -> headMay rest >>= matchSeg
    where matchSeg = mayWhen (withNextSegmentConsumed act) . (segWant ==)

nextMatcher :: MonadRespond m => (T.Text -> m a) -> PathMatcher (m a)
nextMatcher consumer = PathMatcher $ fmap (withNextSegmentConsumed . consumer) . headMay
