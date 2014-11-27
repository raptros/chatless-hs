{-|
Description: process api responses

types and tools for processing responses from the chatless api
-}
{-# LANGUAGE TemplateHaskell #-}
module Chatless.Client.Response where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Network.HTTP.Client as C

import Data.Aeson
import Data.Aeson.TH

-- * types

-- | an error report in the format produced by the respond library.
data ErrorReport = ErrorReport {
    reason :: T.Text,
    message :: Maybe T.Text,
    details :: Maybe Value
} deriving (Eq, Show)

$(deriveJSON defaultOptions ''ErrorReport)

-- | an ApiError has the status, headers, and body; the last is possibly
-- processed into an ErrorReport
data ApiError = ApiError {
    aeStatus :: Status,
    aeHeaders :: ResponseHeaders,
    aeBody :: Either BSL.ByteString ErrorReport
} deriving (Eq, Show)

-- * tools

-- | convert
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = maybe (Left l) Right

-- | process an API response by parsing the body as JSON (either into the
-- expected type if the response is successful, or into an ErrorReport if
-- the response is an error code).
processApiResponse :: FromJSON a => C.Response BSL.ByteString -> Either ApiError a
processApiResponse response
    | statusIsSuccessful rStatus = maybeToEither (mkApiError (Left rBody)) $ decode' rBody -- figure out what to actually do here.
    | otherwise = Left $ mkApiError $ maybeToEither rBody (decode' rBody)
    where
    rStatus = C.responseStatus response
    rHeaders = C.responseHeaders response
    rBody = C.responseBody response
    mkApiError = ApiError rStatus rHeaders

