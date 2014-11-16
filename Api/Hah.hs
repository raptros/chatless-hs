{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Api.Hah where

import Control.Applicative ((<$>), Applicative)
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
import Control.Lens (at, (^.), (&), (?~), (%~), makeLenses)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Web.ReqRes

data CLConfig = CLConfig {
    configServerId  :: ServerId
}

data CLReq = CLReq {
    _clrReqErrorHandlers :: RequestErrorHandlers,
    _clrLocalServer :: ServerId,
    _clrRequest :: Request,
    _clrResponder :: Responder
}

makeLenses ''CLReq

instance HasRequest CLReq where
    getRequest = _clrRequest

instance HasResponder CLReq where
    getResponder = _clrResponder

instance HasRequestErrorHandlers CLReq where
    getRequestErrorHandlers = _clrReqErrorHandlers
    modifyRequestErrorHandlers f = clrReqErrorHandlers %~ f

mkCLR :: RequestErrorHandlers -> CLConfig -> Request -> Responder -> CLReq
mkCLR errHandlers config req responder = CLReq errHandlers (configServerId config) req responder

type CLApi = ReaderT CLReq IO

apiApplication :: CLConfig -> Application
apiApplication conf = handleRequests lifter routeThang 
    where 
    lifter req responder action = runReaderT action (mkCLR defaultRequestErrorHandlers conf req responder)

routeThang :: CLApi ResponseReceived
routeThang = asks (pathInfo . _clrRequest) >>= headTailSafeFold apiRoot firstHandler

apiRoot :: CLApi ResponseReceived
apiRoot = methodRoute $ Map.empty &
        at GET ?~ (respond $ OkJson (object ["location" .= ("here" :: T.Text)])) &
        at PUT ?~ (respond $ OkJson (object ["location" .= ("there" :: T.Text)]))

firstHandler :: T.Text -> [T.Text] -> CLApi ResponseReceived
firstHandler p ps = respond $ DefaultHeaders notImplemented501 (object ["head" .= p, "tail" .= ps])

