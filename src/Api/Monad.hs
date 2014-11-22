{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Api.Monad where

import Data.Monoid ((<>))
import Control.Applicative ((<$>), Applicative, (<|>))
import Model.ID
import Network.Wai
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT, MonadBaseControl, StM, liftBaseWith, defaultLiftBaseWith, restoreM, defaultRestoreM, ComposeSt)
import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Logger
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
import Data.Pool (Pool)
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Api.Config

class (Functor m, MonadIO m, MonadBaseControl IO m) => MonadChatless m where
    getServerId :: m ServerId
    getConn :: m (Pool CLDb)

instance MonadChatless m => MonadChatless (RespondT m) where
    getServerId = lift getServerId
    getConn = lift getConn 

newtype ChatlessT m a = ChatlessT {
    unChatlessT :: LoggingT (ReaderT CLConfig m) a
} deriving (Functor, Applicative, Monad, MonadReader CLConfig, MonadLogger)

runChatlessT :: MonadIO m => ChatlessT m a -> CLConfig -> m a
runChatlessT act conf = runReaderT (runStdoutLoggingT (unChatlessT act)) conf

instance (Functor m, MonadIO m, MonadBaseControl IO m) => MonadChatless (ChatlessT m) where
    getServerId = view clcServerId
    getConn = view clcDb

instance MonadTrans ChatlessT where
    lift = ChatlessT . lift . lift

instance MonadIO m => MonadIO (ChatlessT m) where
    liftIO = ChatlessT . liftIO 

--these next three son of a gun all need UndecidableInstances

instance MonadBase b m => MonadBase b (ChatlessT m) where
    liftBase = liftBaseDefault

-- and these two demand TypeFamilies

instance MonadTransControl ChatlessT where
    newtype StT ChatlessT a = StChatless { unStChatless :: StT (ReaderT CLConfig) (StT LoggingT a) }
    liftWith = \f -> ChatlessT $ liftWith $ \run -> liftWith $ \run' -> f $ liftM StChatless . run' . run . unChatlessT
    restoreT = ChatlessT . restoreT . restoreT . liftM unStChatless

instance MonadBaseControl b m => MonadBaseControl b (ChatlessT m) where
    newtype StM (ChatlessT m) a = StMT { unStMT :: ComposeSt ChatlessT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM   unStMT

onConn :: MonadChatless m => (CLDb -> m a) -> m a
onConn f = getConn >>= withConn f

runTransaction :: MonadChatless m => DbPersist CLDb m a -> m a
runTransaction =  onConn . runDbPersist

onConnNoTransaction :: MonadChatless m => (CLDb -> m a) -> m a
onConnNoTransaction f = getConn >>= withConnNoTransaction f

runQuery :: MonadChatless m => DbPersist CLDb m a -> m a
runQuery = onConnNoTransaction . runDbPersist


type Chatless = ChatlessT IO

runChatless :: Chatless a -> CLConfig -> IO a
runChatless act conf = (runChatlessT act conf)

