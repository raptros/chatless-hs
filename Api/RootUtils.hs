{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, ConstraintKinds #-}
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
import Operations

getCaller :: (MonadHandler m, MonadReader Chatless m) => m UserRef
getCaller = UserCoordKey <$> reader localServer <*>  extractUserId

loadMe :: (MonadHandler m, MonadReader Chatless m, CatchDbConn m cm conn) => m User
loadMe = getCaller >>= loadUser MeNotFound >>= respondOp

getLocalUser :: (MonadHandler m, MonadReader Chatless m, CatchDbConn m cm conn) => UserId -> m User
getLocalUser uid = reader (refLocalUser uid) >>= loadUser UserNotFound >>= respondOp

getAnyUser :: (MonadHandler m, MonadReader Chatless m, CatchDbConn m cm conn) => ServerId -> UserId -> m User
getAnyUser sid uid = do
    lserver <- reader localServer
    unless (sid == lserver) $ sendResponseStatus notImplemented501 $ reasonObject "not_implemented" ["operation" .= ("request_remote_user" :: Text), "server" .= sid]
    getLocalUser uid

refLocalUser :: UserId -> Chatless -> UserRef
refLocalUser = flip $ UserCoordKey . localServer
