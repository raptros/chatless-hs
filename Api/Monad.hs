{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Web.Respond

data CLConfig = CLConfig {
    _clcServerId  :: ServerId
}

makeLenses ''CLConfig

class (Functor m, MonadIO m) => MonadChatless m where
    getServerId :: m ServerId

instance MonadChatless m => MonadChatless (RespondT m) where
    getServerId = lift getServerId

newtype ChatlessT m a = ChatlessT {
    unChatlessT :: ReaderT CLConfig m a
} deriving (Functor, Applicative, Monad, MonadReader CLConfig)

runChatlessT :: ChatlessT m a -> CLConfig -> m a
runChatlessT = runReaderT . unChatlessT

instance (Functor m, MonadIO m) => MonadChatless (ChatlessT m) where
    getServerId = view clcServerId

instance MonadTrans ChatlessT where
    lift = ChatlessT . lift

instance MonadIO m => MonadIO (ChatlessT m) where
    liftIO = ChatlessT . liftIO 

--these next three son of a gun all need UndecidableInstances

instance MonadBase b m => MonadBase b (ChatlessT m) where
    liftBase = liftBaseDefault

-- and these two demand TypeFamilies

instance MonadTransControl ChatlessT where
    newtype StT ChatlessT a = StChatless { unStChatless :: StT (ReaderT CLConfig) a }
    liftWith = defaultLiftWith ChatlessT unChatlessT StChatless
    restoreT = defaultRestoreT ChatlessT unStChatless

instance MonadBaseControl b m => MonadBaseControl b (ChatlessT m) where
    newtype StM (ChatlessT m) a = StMT { unStMT :: ComposeSt ChatlessT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM   unStMT


