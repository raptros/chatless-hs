{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, ConstraintKinds #-}
module Api.RootUtils where

import Control.Monad (unless)
import Control.Applicative ((<$>), (<*>))

import Api.Utils (extractUserId, respondOpError, respondOp)
import Api.Root (Chatless, localServer)
import Yesod.Core (MonadHandler, HandlerSite, getYesod)

import Model.ID (ServerId, UserId)
import Model.User (UserRef, User, Key(UserCoordKey))
import qualified Operations as Op

getCaller :: (MonadHandler m, HandlerSite m ~ Chatless) => m UserRef
getCaller = UserCoordKey <$> (localServer <$> getYesod) <*>  extractUserId

loadMe :: (MonadHandler m, HandlerSite m ~ Chatless, Op.CatchDbConn m cm conn) => m User
loadMe = getCaller >>= Op.loadUser Op.MeNotFound >>= respondOp

getLocalUser :: (MonadHandler m, HandlerSite m ~ Chatless, Op.CatchDbConn m cm conn) => UserId -> m User
getLocalUser uid = (refLocalUser uid) <$> getYesod >>= Op.loadUser Op.UserNotFound >>= respondOp

getAnyUser :: (MonadHandler m, HandlerSite m ~ Chatless, Op.CatchDbConn m cm conn) => ServerId -> UserId -> m User
getAnyUser sid uid = do
    lserver <- localServer <$> getYesod
    unless (sid == lserver) $ respondOpError (Op.NotImplemented Op.GetRemoteUser)
    getLocalUser uid

refLocalUser :: UserId -> Chatless -> UserRef
refLocalUser = flip $ UserCoordKey . localServer
