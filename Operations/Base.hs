{-# LANGUAGE ConstraintKinds, OverloadedStrings, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}
module Operations.Base where

import Control.Monad.Catch (MonadThrow, MonadCatch, Exception, throwM, try)
import Control.Monad.Trans.Class (lift)
import Database.Groundhog (DbPersist, PersistBackend)
import Database.Groundhog.Generic (HasConn, runDb)
import Data.Typeable (Typeable)

import Model.ID (TopicId, MessageId)
import qualified Model.User as Ur
import qualified Model.Topic as Tp
import qualified Model.Message as Msg

infixr 9 .*
(.*) :: (b -> a) -> (d -> c -> b) -> d -> c -> a
(.*) = (.) . (.)

infixr 9 .**
(.**) :: (b -> a) -> (e -> d -> c -> b) -> e -> d -> c -> a
(.**) = (.) . (.) . (.)

infixr 9 .***
(.***) :: (b -> a) -> (f -> e -> d -> c -> b) -> f -> e -> d -> c -> a
(.***) = (.) . (.) . (.) . (.)

data OpType = 
    ReadTopic |
    SetMemberMode |
    SetTopicMode |
    SendMessage |
    SetBanner |
    InviteUser |
    GetRemoteUser
    deriving (Show, Typeable)

data OpError = 
    MeNotFound Ur.UserRef |
    TopicNotFound Tp.TopicRef |
    UserNotFound Ur.UserRef |
    MemberNotFound Tp.TopicRef Ur.UserRef |
    MessageNotFound Msg.MessageRef |
    OperationDenied OpType |
    IdInUse Tp.TopicRef |
    GenerateIdFailed Ur.UserRef [TopicId] |
    GenerateMessageIdFailed Tp.TopicRef [MessageId] |
    MessageIdInUse Msg.MessageRef |
    LoadMessageFailed Msg.MessageRef |
    AlreadyMember Tp.TopicRef Ur.UserRef |
    NotImplemented OpType
    deriving (Show, Typeable)

instance Exception OpError

getOrThrow :: (Exception e, MonadThrow m) => e -> Maybe a -> m a
getOrThrow err = maybe (throwM err) return

throwEither :: (Exception e, MonadThrow m) => (l -> e) -> Either l r -> m r
throwEither f = either (throwM . f) return

throwEitherConst :: (Exception e, MonadThrow m) => e -> Either l r -> m r
throwEitherConst = throwEither . const

instance MonadThrow m => MonadThrow (DbPersist conn m) where
    throwM = lift . throwM

type CatchDbConn m cm conn = (HasConn m cm conn, MonadCatch m, PersistBackend (DbPersist conn m))

runOp :: (HasConn m cm conn, MonadCatch m) => DbPersist conn m a -> m (Either OpError a)
runOp = try . runDb
