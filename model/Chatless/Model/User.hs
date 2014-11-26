{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Chatless.Model.User (
                  User(..),
                  UserRef,
                  Key(UserCoordKey),
                  getRefFromUser,
                  userRefServer,
                  userRefUser,
                  UserConstructor(UserConstructor)
                  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.HashMap.Strict as H

import Data.Aeson ((.=), Object, (.:), withObject, FromJSON, parseJSON, ToJSON, toJSON, Value(Object))
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)

import Database.Groundhog (Key, Unique)
import Database.Groundhog.TH (mkPersist, defaultCodegenConfig, groundhog)

import Chatless.Model.ID (ServerId, UserId, TopicId)
import Chatless.Model.Utils (dropAndLowerHead)

data User = User {
    userServer :: ServerId,
    userId :: UserId,
    userAbout :: TopicId, -- 
    userInvite :: TopicId
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 4 } ''User)

mkPersist defaultCodegenConfig [groundhog|
- entity: User
  autoKey: null
  keys:
    - name: UserCoord
      default: true
  constructors:
    - name: User
      uniques:
        - name: UserCoord
          type: primary
          fields: [userServer, userId]
      fields:
        - name: userServer
          dbName: server
        - name: userId
          dbName: id
        - name: userAbout
          dbName: about
        - name: userInvite
          dbName: invite
|]

type UserRef = (Key User (Unique UserCoord))

getRefFromUser :: User -> UserRef
getRefFromUser = UserCoordKey <$> userServer <*> userId

userRefServer :: UserRef -> ServerId
userRefServer (UserCoordKey s _) = s

userRefUser :: UserRef -> UserId
userRefUser (UserCoordKey _ u) = u

userRefObject :: UserRef -> Object
userRefObject (UserCoordKey sid uid) = H.fromList ["server" .= sid, "user" .= uid]

instance ToJSON (Key User (Unique UserCoord)) where
    toJSON = Object . userRefObject

instance FromJSON (Key User (Unique UserCoord)) where
    parseJSON = withObject "UserCoordKey" $ \v -> UserCoordKey <$> v .: "server" <*> v .: "user"
