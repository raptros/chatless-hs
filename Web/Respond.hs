{-|
Description: main module for library. exports everything.

Contains the tools you need to build WAI 'Application's.
-}
{-# LANGUAGE RankNTypes #-}
module Web.Respond (
                   -- * building an app
                  respondApp,
                  respondAppDefault,
                  -- * the modules
                  module Web.Respond.Types,
                  module Web.Respond.Monad,
                  module Web.Respond.Response,
                  module Web.Respond.Request,
                  module Web.Respond.Path,
                  ) where

import Network.Wai
import Control.Monad.IO.Class (MonadIO)

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response
import Web.Respond.Request
import Web.Respond.Path

-- | it's 'respondApp' with 'defaultRequestErrorHandlers' passed in.
respondAppDefault :: MonadIO m => (forall a. m a -> IO a)  -> RespondT m ResponseReceived -> Application 
respondAppDefault = respondApp defaultRequestErrorHandlers

-- | build an 'Network.Wai.Application' from a 'RespondT' based handler.
respondApp :: MonadIO m  => RequestErrorHandlers -- ^ however you want errors handled
              -> (forall a. m a -> IO a) -- ^ how to unpeel your monad to 'IO'
              -> RespondT m ResponseReceived -- ^ your handler.
              -> Application -- ^ give this to warp or something
respondApp handlers lifter api req res = lifter (runRespondT api handlers req res)
