{-|
Description: chatless client monad transformer

the monad transformer that lets you call to the chatless API!
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Chatless.Client.Monad where

import Control.Applicative
import Control.Monad.Reader 
import Control.Monad.Catch
import Control.Monad.Except

import Chatless.Client.Session
import Chatless.Client.Response


class (Functor m, MonadIO m, MonadError ResponseError m) => MChatlessClient m where
    getSession :: m Session

newtype CLClientT m a = CLClientT {
    unCLClientT :: ExceptT ResponseError (ReaderT Session m) a
} deriving (Functor, Applicative, Monad, MonadReader Session, MonadError ResponseError)

runCLClientT :: (Functor m, MonadIO m) => CLClientT m a -> Session -> m (Either ResponseError a)
runCLClientT = runReaderT . runExceptT . unCLClientT

instance (Functor m, MonadIO m) => MChatlessClient (CLClientT m) where
    getSession = ask

instance MonadTrans CLClientT where
    lift = CLClientT . lift . lift

instance MonadIO m => MonadIO (CLClientT m) where
    liftIO = CLClientT . liftIO 

instance MonadThrow m => MonadThrow (CLClientT m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (CLClientT m) where
    catch act h = CLClientT $ catch (unCLClientT act) $ \e -> unCLClientT (h e)

