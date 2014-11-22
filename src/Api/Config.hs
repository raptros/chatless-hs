{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Api.Config where

import Data.Monoid ((<>))
import Control.Applicative ((<$>), Applicative, (<|>))
import Model.ID
import Network.Wai
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT, MonadBaseControl, StM, liftBaseWith, defaultLiftBaseWith, restoreM, defaultRestoreM, ComposeSt)
import Control.Monad.Trans.Class
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
import Data.Pool (Pool)
import Web.Respond
import Database.Groundhog.Core (ConnectionManager(..))
import Database.Groundhog.Sqlite

type CLDb = Sqlite

data CLConfig = CLConfig {
    _clcServerId  :: ServerId,
    _clcDb :: Pool CLDb
}

makeLenses ''CLConfig

instance ConnectionManager CLConfig CLDb where
    withConn f = withConn f . _clcDb 
    withConnNoTransaction f = withConnNoTransaction f . _clcDb 

