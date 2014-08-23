{-# LANGUAGE ConstraintKinds, OverloadedStrings, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}
module Operations.Base where

import Model.ID
import Model.User
import Model.Topic
import Model.Message
import Control.Monad.Except
import Control.Monad.Catch
import Database.Groundhog
import Database.Groundhog.Generic
import Data.Typeable

data OpType = 
    ReadTopic |
    SetMemberMode |
    SetTopicMode |
    SendMessage |
    SetBanner |
    InviteUser
    deriving (Show, Typeable)

data OpError = 
    MeNotFound UserRef |
    TopicNotFound TopicRef |
    UserNotFound UserRef |
    MemberNotFound TopicRef UserRef |
    MessageNotFound MessageRef |
    OperationDenied OpType |
    IdInUse TopicRef |
    GenerateIdFailed UserRef [TopicId] |
    GenerateMessageIdFailed TopicRef [MessageId] |
    MessageIdInUse MessageRef |
    LoadMessageFailed MessageRef |
    AlreadyMember TopicRef UserRef
    deriving (Show, Typeable)

instance Exception OpError

getOrThrow :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
getOrThrow err = maybe (throwM err) return

throwEither :: (Exception e, MonadThrow m) => (l -> e) -> Either l r -> m r
throwEither f = either (throwM . f) return

throwEitherConst :: (Exception e, MonadThrow m) => e -> Either l r -> m r
throwEitherConst = throwEither . const

-- apparently this instance is no longer necessary
{-
instance MonadLogger m => MonadLogger (ExceptT e m) where
    monadLoggerLog a b c d = Trans.lift $ monadLoggerLog a b c d
-}

instance MonadThrow m => MonadThrow (DbPersist conn m) where
    throwM = lift . throwM

type CatchDbConn m cm conn = (HasConn m cm conn, MonadCatch m, PersistBackend (DbPersist conn m))

runOp :: (HasConn m cm conn, MonadCatch m) => DbPersist conn m a -> m (Either OpError a)
runOp op = try $ runDb op
