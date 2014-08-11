{-# LANGUAGE ConstraintKinds, OverloadedStrings, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}
module Operations.Base where

import Model.ID
import Model.User
import Model.Topic
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Catch
import Database.Groundhog
import Database.Groundhog.Generic
import qualified Data.Text as T
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Random
import Data.Typeable

data ApiOperation = 
    ReadMembers |
    SetMemberMode
    deriving (Show, Typeable)

data ApiError = 
    MeNotFound UserRef |
    TopicNotFound TopicRef |
    UserNotFound UserRef |
    MemberNotFound TopicRef UserRef |
    OperationDenied ApiOperation |
    IdInUse TopicRef |
    GenerateIdFailed UserRef [TopicId]
    deriving (Show, Typeable)

instance Exception ApiError

getOrThrow :: (MonadError e m) => e -> Maybe a -> m a
getOrThrow err = maybe (throwError err) return

getOrThrow' :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
getOrThrow' err = maybe (throwM err) return
--randomId :: MonadRandom m => Int -> m T.Text

throwEither :: (Exception e, MonadThrow m) => (l -> e) -> Either l r -> m r
throwEither f = either (throwM . f) return

throwEitherConst :: (Exception e, MonadThrow m) => e -> Either l r -> m r
throwEitherConst = throwEither . const

instance MonadLogger m => MonadLogger (ExceptT e m) where
    monadLoggerLog a b c d = Trans.lift $ monadLoggerLog a b c d

instance MonadThrow m => MonadThrow (DbPersist conn m) where
    throwM = lift . throwM

type CatchDbConn m cm conn = (HasConn m cm conn, MonadCatch m, PersistBackend (DbPersist conn m))

runOp :: (HasConn m cm conn, MonadCatch m) => DbPersist conn m a -> m (Either ApiError a)
runOp op = try $ runDb op
