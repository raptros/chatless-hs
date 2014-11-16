{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Web.ReqRes (
                  handleRequests,
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

handleRequests :: (HasRequestErrorHandlers t, MonadIO m, MonadReader t m) => (forall a. Request -> Responder -> m a -> IO a) -> m ResponseReceived -> Application
handleRequests lifter api request responder = lifter request responder api
