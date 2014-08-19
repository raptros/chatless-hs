{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.Topic where

import Data.Text hiding (drop)
import qualified Data.Char as C

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Pair, Parser, typeMismatch)

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.TH
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)

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

topicRefUserRef :: TopicRef -> UserRef
topicRefUserRef (TopicCoordKey sid uid _) = UserCoordKey sid uid

topicRefId :: TopicRef -> TopicId
topicRefId (TopicCoordKey _ _ tid) = tid

topicRefObject :: TopicRef -> Object
topicRefObject (TopicCoordKey sid uid tid) = H.fromList ["server" .= sid, "user" .= uid, "topic" .= tid]

instance ToJSON (Key Topic (Unique TopicCoord)) where
    toJSON = Object . topicRefObject

topicRefFromObject :: Object -> Parser TopicRef
topicRefFromObject v = TopicCoordKey <$> v .: "server" <*> v .: "user" <*> v .: "topic"

instance FromJSON (Key Topic (Unique TopicCoord)) where
    parseJSON = withObject "TopicCoordKey" topicRefFromObject

fromUserRef :: TopicId -> UserRef -> TopicRef
fromUserRef tid (UserCoordKey sid uid) = TopicCoordKey sid uid tid

isCreator :: UserRef -> Topic -> Bool
isCreator (UserCoordKey sid uid) t = (sid == topicServer t) && (uid == topicUser t)

initializeTopic :: UserRef -> TopicId -> TopicCreate -> Topic
initializeTopic caller tid (TopicCreate _ mBanner mInfo mMode) = Topic {
    topicServer = userRefServer caller,
    topicUser = userRefUser caller,
    topicId = tid,
    topicBanner = fromMaybe "" mBanner,
    topicInfo = fromMaybe storableEmpty mInfo,
    topicMode = fromMaybe defaultTopicMode mMode
}

