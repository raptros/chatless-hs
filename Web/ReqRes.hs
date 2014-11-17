{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Web.ReqRes (
                  handleRequests,
                  handleRequests',
                  module Web.ReqRes.Types,
                  module Web.ReqRes.Response,
                  module Web.ReqRes.Request
                  ) where

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

import Web.ReqRes.Types
import Web.ReqRes.Response
import Web.ReqRes.Request

handleRequests :: MonadIO m => (forall a. m a -> IO a) -> RespondT m ResponseReceived -> Application
handleRequests = handleRequests' defaultRequestErrorHandlers

handleRequests' :: MonadIO m => RequestErrorHandlers -> (forall a. m a -> IO a) -> RespondT m ResponseReceived -> Application
handleRequests' handlers lifter api req res = lifter (runRespondT api handlers req res)
