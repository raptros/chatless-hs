{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.Topic where

import Data.Text hiding (drop)
import qualified Data.Char as C

import Data.Aeson
import Data.Aeson.TH

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.TH

import Model.User
import Model.Utils
import Model.ID
import Model.StorableJson

import Control.Applicative

data TopicMode = TopicMode {
    readable :: Bool,
    writable :: Bool,
    muted :: Bool,
    membersOnly :: Bool,
    authenticatedOnly :: Bool
} deriving (Show, Eq)

defaultTopicMode :: TopicMode
defaultTopicMode = TopicMode True True False True True

$(deriveJSON defaultOptions ''TopicMode)

data TopicCreate = TopicCreate {
    createId :: Maybe TopicId,
    createBanner :: Maybe String,
    createInfo :: Maybe StorableJson,
    createMode :: Maybe TopicMode
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 6} ''TopicCreate)

data Topic = Topic {
    topicServer :: ServerId,
    topicUser :: UserId,
    topicId :: TopicId,
    topicBanner :: String,
    topicInfo :: StorableJson,
    topicMode :: TopicMode
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 5 } ''Topic)

mkPersist defaultCodegenConfig [groundhog|
- embedded: TopicMode
- entity: Topic
  autoKey: null
  keys:
    - name: TopicCoord
      default: true
  constructors:
    - name: Topic
      uniques:
        - name: TopicCoord
          type: primary
          fields: [topicServer, topicUser, topicId]
      fields:
        - name: topicServer
          dbName: server
        - name: topicUser
          dbName: user
        - name: topicId
          dbName: id
        - name: topicBanner
          dbName: banner
        - name: topicInfo
          dbName: info
        - name: topicMode
          dbName: mode
|]

instance NeverNull TopicMode

type TopicRef = Key Topic (Unique TopicCoord)

instance ToJSON (Key Topic (Unique TopicCoord)) where
    toJSON (TopicCoordKey sid uid tid) = object ["server" .= sid, "user" .= uid, "topic" .= tid]

instance FromJSON (Key Topic (Unique TopicCoord)) where
    parseJSON = withObject "TopicCoordKey" $ \v -> TopicCoordKey <$> v .: "server" <*> v .: "user" <*> v .: "topic"

fromUserRef :: TopicId -> UserRef -> TopicRef
fromUserRef tid (UserCoordKey sid uid) = TopicCoordKey sid uid tid

isCreator :: UserRef -> Topic -> Bool
isCreator (UserCoordKey sid uid) t = (sid == topicServer t) && (uid == topicUser t)
