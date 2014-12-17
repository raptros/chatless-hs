{-|
Description: process api responses

types and tools for processing responses from the chatless api
-}
{-# LANGUAGE TemplateHaskell #-}
module Chatless.Client.Response where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import qualified Network.HTTP.Client as C
import Data.Bifunctor
import Control.Lens ((&))

import Data.Aeson
import Data.Aeson.TH

import qualified Chatless.Model.Message as Msg
import qualified Chatless.Model.Topic as Tp

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

data ParseError = ParseError {
    peStatus :: Status,
    peHeaders :: ResponseHeaders,
    peBody :: BSL.ByteString,
    peMsg :: String
} deriving (Eq, Show)

data ResponseError = ResponseApiError ApiError | ResponseParseError ParseError deriving (Eq, Show)

type ResponseResult a = Either ResponseError a

-- * tools

-- | convert
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = maybe (Left l) Right

responseAsApiError :: C.Response BSL.ByteString -> ApiError
responseAsApiError = ApiError <$> C.responseStatus <*> C.responseHeaders <*> (maybeToEither <*> decode') . C.responseBody

apiErrorResponseResult :: C.Response BSL.ByteString -> ResponseResult a
apiErrorResponseResult = Left . ResponseApiError . responseAsApiError

responseAsParseError :: C.Response BSL.ByteString -> String -> ParseError
responseAsParseError = ParseError <$> C.responseStatus <*> C.responseHeaders  <*> C.responseBody

parseResponseJSON :: FromJSON a => C.Response BSL.ByteString -> ResponseResult a
parseResponseJSON =  first <$>  ((ResponseParseError .) . responseAsParseError) <*> (eitherDecode' . C.responseBody)

type StatusInterpreter a = (Status, C.Response BSL.ByteString -> ResponseResult a)

resultByStatus :: [StatusInterpreter a] -> C.Response BSL.ByteString -> ResponseResult a
resultByStatus interpreters = maybe <$> apiErrorResponseResult <*> (&) <*> flip lookup interpreters . C.responseStatus

class ResponseInterpreter a where
    interpretResponse :: C.Response BSL.ByteString -> ResponseResult a


newtype OkJsonResponse a = OkJsonResponse { okJsonBody :: a } deriving (Eq, Show)

instance FromJSON a => ResponseInterpreter (OkJsonResponse a) where
    interpretResponse = resultByStatus [(ok200, fmap OkJsonResponse . parseResponseJSON)]

newtype TopicOpResult = TopicOpResult { topicOpCreatedMessages :: [Msg.MessageRef] } deriving (Eq, Show)

topicOpNoChanges :: TopicOpResult -> Bool
topicOpNoChanges = null . topicOpCreatedMessages

instance ResponseInterpreter TopicOpResult where
    interpretResponse = resultByStatus [
        (noContent204, const $ Right $ TopicOpResult []),
        (ok200, fmap TopicOpResult . parseResponseJSON)
        ]

newtype TopicCreateResult = TopicCreateResult { topicCreated :: Tp.TopicRef } deriving (Eq, Show)

instance ResponseInterpreter TopicCreateResult where
    interpretResponse = resultByStatus [
        (created201, fmap TopicCreateResult . parseResponseJSON)
        ]
