{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
module Api.RootUtils where

import Data.Aeson
import Data.Text
import Api.Utils
import Api.Root
import Yesod.Core

import Model.ID
import Model.User
import Model.Topic
import Model.TopicMember
import Model.StorableJson
import Database.Groundhog
import Database.Groundhog.Generic (runDb, HasConn)
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe
import Network.HTTP.Types

getCaller :: (MonadHandler m, MonadReader Chatless m) => m UserRef
getCaller = UserCoordKey <$> reader localServer <*>  extractUserId


getLocalUser :: (MonadHandler m, MonadReader Chatless m, HasConn m cm conn, PersistBackend (DbPersist conn m)) => UserId -> m User
getLocalUser uid = do
        sid <- reader localServer
        mUser <- runDb $ getBy $ UserCoordKey sid uid
        maybe (userNotFound sid uid) return mUser

getAnyUser :: (MonadHandler m, MonadReader Chatless m, HasConn m cm conn, PersistBackend (DbPersist conn m)) => ServerId -> UserId -> m User
getAnyUser sid uid = do
    lserver <- reader localServer
    unless (sid == lserver) $ sendResponseStatus notImplemented501 $ reasonObject "not_implemented" ["operation" .= ("request_remote_user" :: Text), "server" .= sid]
    getLocalUser uid

loadMe :: (MonadHandler m, MonadReader Chatless m, HasConn m cm conn, PersistBackend (DbPersist conn m)) => m User
loadMe = do
    meRef <- getCaller
    mUser <- runDb $ getBy meRef
    maybe (meNotPresent meRef) return mUser

refLocalUser :: UserId -> Chatless -> UserRef
refLocalUser = flip $ UserCoordKey . localServer
