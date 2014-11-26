{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Api.Monad where

import Control.Applicative (Applicative)
import Model.ID
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT )
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT, MonadBaseControl, StM, liftBaseWith, defaultLiftBaseWith, restoreM, defaultRestoreM, ComposeSt)
import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Lens (view)
import Web.Respond
import Data.Pool (Pool)
import Database.Groundhog
import Database.Groundhog.Core
import Api.Config
import Control.Monad.Catch

class (Functor m, MonadIO m, MonadBaseControl IO m) => MonadChatless m where
    getServerId :: m ServerId
    getConn :: m (Pool CLDb)

instance MonadChatless m => MonadChatless (RespondT m) where
    getServerId = lift getServerId
    getConn = lift getConn 

instance MonadChatless m => MonadChatless (ExceptT e m) where
    getServerId = lift getServerId
    getConn = lift getConn 

instance MonadChatless m => MonadChatless (MaybeT m) where
    getServerId = lift getServerId
    getConn = lift getConn 

instance MonadChatless m => MonadChatless (NoLoggingT m) where
    getServerId = lift getServerId
    getConn = lift getConn

instance MonadChatless m => MonadChatless (LoggingT m) where
    getServerId = lift getServerId
    getConn = lift getConn

newtype ChatlessT m a = ChatlessT {
    unChatlessT :: LoggingT (ReaderT CLConfig m) a
} deriving (Functor, Applicative, Monad, MonadReader CLConfig, MonadLogger)

runChatlessT :: MonadIO m => ChatlessT m a -> CLConfig -> m a
runChatlessT act = runReaderT (runStdoutLoggingT (unChatlessT act)) 

instance (Functor m, MonadIO m, MonadBaseControl IO m) => MonadChatless (ChatlessT m) where
    getServerId = view clcServerId
    getConn = view clcDb

instance MonadTrans ChatlessT where
    lift = ChatlessT . lift . lift

instance MonadIO m => MonadIO (ChatlessT m) where
    liftIO = ChatlessT . liftIO 

instance MonadThrow m => MonadThrow (ChatlessT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (ChatlessT m) where
    catch act h = ChatlessT $ catch (unChatlessT act) $ \e -> unChatlessT (h e)

--these next three son of a gun all need UndecidableInstances

instance MonadBase b m => MonadBase b (ChatlessT m) where
    liftBase = liftBaseDefault

-- and these two demand TypeFamilies

instance MonadTransControl ChatlessT where
    newtype StT ChatlessT a = StChatless { unStChatless :: StT (ReaderT CLConfig) (StT LoggingT a) }
    liftWith f = ChatlessT $ liftWith $ \run -> liftWith $ \run' -> f $ liftM StChatless . run' . run . unChatlessT
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

type CLQuery m a = DbPersist CLDb m a

runQuery :: (MonadChatless m) => DbPersist CLDb (LoggingT m) a -> m a
runQuery q = runStdoutLoggingT $ onConnNoTransaction (runDbPersist q)

--runQueryNoLog :: MonadChatless m => DbPersist CLDb (NoLoggingT m) a -> m a
--runQueryNoLog q = runNoLoggingT (runQuery q)

type Chatless = ChatlessT IO

runChatless :: Chatless a -> CLConfig -> IO a
runChatless = runChatlessT 

