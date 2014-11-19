{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Web.ReqRes (
                  handleRequests,
                  handleRequests',
                  module Web.ReqRes.Types,
                  module Web.ReqRes.Response,
                  module Web.ReqRes.Request
                  ) where

import Network.Wai
import Control.Monad.IO.Class (MonadIO)

import Web.ReqRes.Types
import Web.ReqRes.Response
import Web.ReqRes.Request

handleRequests :: MonadIO m => (forall a. m a -> IO a) -> RespondT m ResponseReceived -> Application
handleRequests = handleRequests' defaultRequestErrorHandlers

handleRequests' :: MonadIO m => RequestErrorHandlers -> (forall a. m a -> IO a) -> RespondT m ResponseReceived -> Application
handleRequests' handlers lifter api req res = lifter (runRespondT api handlers req res)
