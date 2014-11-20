{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Web.Respond (
                  respondApp,
                  respondAppDefault,
                  module Web.Respond.Types,
                  module Web.Respond.Response,
                  module Web.Respond.Request
                  ) where

import Network.Wai
import Control.Monad.IO.Class (MonadIO)

import Web.Respond.Types
import Web.Respond.Response
import Web.Respond.Request

respondAppDefault :: MonadIO m => (forall a. m a -> IO a) -> RespondT m ResponseReceived -> Application
respondAppDefault = respondApp defaultRequestErrorHandlers

respondApp :: MonadIO m => RequestErrorHandlers -> (forall a. m a -> IO a) -> RespondT m ResponseReceived -> Application
respondApp handlers lifter api req res = lifter (runRespondT api handlers req res)
