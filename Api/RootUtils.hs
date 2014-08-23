{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, ConstraintKinds #-}
module Api.RootUtils where

import Data.Aeson
import Data.Text
import Api.Utils
import Api.Root
import Yesod.Core

import Model.ID
import Model.User
import Control.Monad.Reader
import Control.Applicative
import Network.HTTP.Types
import Operations

getCaller :: (MonadHandler m, HandlerSite m ~ Chatless) => m UserRef
getCaller = UserCoordKey <$> (localServer <$> getYesod) <*>  extractUserId

loadMe :: (MonadHandler m, HandlerSite m ~ Chatless, CatchDbConn m cm conn) => m User
loadMe = getCaller >>= loadUser MeNotFound >>= respondOp

getLocalUser :: (MonadHandler m, HandlerSite m ~ Chatless, CatchDbConn m cm conn) => UserId -> m User
getLocalUser uid = (refLocalUser uid) <$> getYesod >>= loadUser UserNotFound >>= respondOp

getAnyUser :: (MonadHandler m, HandlerSite m ~ Chatless, CatchDbConn m cm conn) => ServerId -> UserId -> m User
getAnyUser sid uid = do
    lserver <- localServer <$> getYesod
    unless (sid == lserver) $ sendResponseStatus notImplemented501 $ reasonObject "not_implemented" ["operation" .= ("request_remote_user" :: Text), "server" .= sid]
    getLocalUser uid

refLocalUser :: UserId -> Chatless -> UserRef
refLocalUser = flip $ UserCoordKey . localServer
