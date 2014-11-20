{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Api.Hah where

import Data.Monoid ((<>))
import Control.Applicative ((<$>), Applicative, (<|>))
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
import Control.Monad.Reader.Class (MonadReader)
import Control.Lens (at, (^.), (&), (?~), (%~), makeLenses, view, to)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Web.Respond

data CLConfig = CLConfig {
    configServerId  :: ServerId
}

type CLApi = RespondT (ReaderT CLConfig IO)

apiApplication :: CLConfig -> Application
apiApplication conf = respondAppDefault (`runReaderT` conf) routeThang 

routeThang :: CLApi ResponseReceived
routeThang = matchPath $
    path endOrSlash apiRoot <|> 
    path (seg "assemble" ) assembleHandler <|>
    path (seg "number" </> value </> seg "string" </> value </> endOrSlash) numHandler
    where
    numHandler :: Integer -> T.Text -> CLApi ResponseReceived
    numHandler num t = respond $ OkJson $ object ["text" .= t, "num" .= num]

assembleHandler :: CLApi ResponseReceived
assembleHandler = matchPath $
    path endOrSlash (respond $ DefaultHeaders notImplemented501 (object ["awaiting" .= ("stray kitten" :: T.Text)])) <|>
    path value firstHandler

apiRoot :: CLApi ResponseReceived
apiRoot = matchMethod $
    onGET (respond $ OkJson (object ["location" .= ("here" :: T.Text)])) <>
    onPUT (respond $ OkJson (object ["location" .= ("there" :: T.Text)]))

firstHandler :: T.Text -> CLApi ResponseReceived
firstHandler p = do 
    ps <- getUnconsumedPath
    respond $ DefaultHeaders notImplemented501 (object ["head" .= p, "tail" .= ps])

