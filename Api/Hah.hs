{-# LANGUAGE OverloadedStrings #-}
module Api.Hah where

import Control.Applicative ((<$>))
import Model.ID
import Network.Wai
import Control.Monad.Trans.Reader
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

data CLConfig = CLConfig {
    configServerId  :: ServerId
}

data CLReq = CLReq {
    localServer :: ServerId,
    request :: Request
}

configToReq :: CLConfig -> Request -> CLReq
configToReq config req = CLReq {
    localServer = configServerId config,
    request = req
}    

type CLApi = ReaderT CLReq IO

runCLApi :: CLApi a -> CLConfig -> Request -> IO a
runCLApi api conf req = runReaderT api (configToReq conf req)


apiApplication :: CLConfig -> Application
apiApplication base req respond = runCLApi routeThang base req >>= respond

routeThang :: CLApi Response
routeThang = asks (pathInfo . request) >>= headTailSafeFold apiRoot firstHandler

apiRoot :: CLApi Response
apiRoot = methodRoute $ Map.empty &
        at GET ?~ return (responseJson ok200 [] (object ["location" .= ("here" :: T.Text)]))

firstHandler :: T.Text -> [T.Text] -> CLApi Response
firstHandler p ps = return $ responseJson notImplemented501 [] (object ["head" .= p, "tail" .= ps])

methodRoute :: Map.Map StdMethod (CLApi Response) -> CLApi Response
methodRoute = methodRoute' stdUnsupportedMethod stdUnknownMethod

methodRoute' :: ([StdMethod] -> StdMethod -> CLApi Response) -> ([StdMethod] -> BS.ByteString -> CLApi Response) -> Map.Map StdMethod (CLApi Response) -> CLApi Response
methodRoute' unsupported unknown dispatcher = asks (requestMethod . request) >>= either (unknown supported) selectMethod . parseMethod
    where
    supported = Map.keys dispatcher
    selectMethod mth = fromMaybe (unsupported supported mth) (dispatcher ^. at mth)

stdUnsupportedMethod :: [StdMethod] -> StdMethod -> CLApi Response
stdUnsupportedMethod supported tried = return $ responseMethodNotAllowedStd supported (object ["tried" .= decodeUtf8 (renderStdMethod tried), "standard" .= True])

stdUnknownMethod :: [StdMethod] -> BS.ByteString -> CLApi Response 
stdUnknownMethod supported tried = return $ responseMethodNotAllowedStd supported (object ["tried" .= decodeUtf8 tried, "standard" .= False])

responseJson :: ToJSON a => Status -> ResponseHeaders -> a -> Response
responseJson s hs a = responseLBS s ((hContentType, "application/json") : hs) (encode a)

responseMethodNotAllowedStd :: ToJSON a => [StdMethod] -> a -> Response
responseMethodNotAllowedStd = responseMethodNotAllowed . fmap renderStdMethod

responseMethodNotAllowed :: ToJSON a => [Method] -> a -> Response
responseMethodNotAllowed allowed = responseJson methodNotAllowed405 [("Allowed", allowedStr)]
    where allowedStr = BS.intercalate ", " allowed

-- wat
headTailSafeFold :: b -> (a -> [a] -> b) -> [a] -> b
headTailSafeFold def _ [] = def
headTailSafeFold _ f (a:as) = f a as
