{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Web.ReqRes.Response where

import Control.Applicative ((<$>))
import Network.Wai
import Data.Aeson
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Data.Text as T
import Control.Lens (view)
import Control.Monad (join)

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

defaultRequestErrorHandlers :: RequestErrorHandlers
defaultRequestErrorHandlers = RequestErrorHandlers defaultUnsupportedMethodHandler defaultUnmatchedPathHandler defaultPathParseFailedHandler

defaultUnsupportedMethodHandler :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived
defaultUnsupportedMethodHandler allowed _ = respond $ EmptyBody methodNotAllowed405 [("Allowed", allowedStr)]
    where allowedStr = BS.intercalate ", " (renderStdMethod <$> allowed)

defaultUnmatchedPathHandler :: MonadRespond m => m ResponseReceived
defaultUnmatchedPathHandler = respond $ EmptyBody status404 []

defaultPathParseFailedHandler :: MonadRespond m => [T.Text] -> m ResponseReceived
defaultPathParseFailedHandler failedOn = respond $ DefaultHeaders badRequest400 ["badPath" .= failedOn]

handleUnsupportedMethod :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived
handleUnsupportedMethod supported unsupported = getREH (view rehUnsupportedMethod) >>= \handler -> handler supported unsupported

handleUnmatchedPath :: MonadRespond m => m ResponseReceived
handleUnmatchedPath = join (getREH (view rehUnmatchedPath))

handlePathParseFailed :: MonadRespond m => [T.Text] -> m ResponseReceived
handlePathParseFailed parts = getREH (view rehPathParseFailed) >>= \handler -> handler parts

