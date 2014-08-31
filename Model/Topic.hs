{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses, FunctionalDependencies #-}
module Model.Topic (
    TopicMode(..),
    defaultTopicMode,
    aboutTopicMode,
    inviteTopicMode,
    Topic(..),
    TopicCreate(..),
    TopicRef,
    Field(..),
    TopicCoord(..),
    Key(TopicCoordKey),
    isCreator,
    fromUserRef,
    topicRefUserRef,
    TopicConstructor(TopicConstructor),
    topicRefId,
    initializeTopic,
    topicRefObject,
    topicRefFromObject,
    TopicModeUpdate(..),
    resolveTopicModeUpdateMay
    ) where

import Control.Applicative ((<$>), (<*>))

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

import Data.Aeson (ToJSON, FromJSON, (.=), (.:), toJSON, parseJSON, Object, Value(..), withObject)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Types (Parser)

import Database.Groundhog.Core (Key, Field, Unique)
import Database.Groundhog.TH (mkPersist, defaultCodegenConfig, groundhog)

import Control.Lens.TH (makeLensesWith, lensRules, lensField)
import Control.Lens.Operators ((&), (.~), (^.))

import Utils ((.*))

import Model.Utils (lensName, dropAndLowerHead, (^.=?), runUpdateR, asMaybe)
import Model.ID (ServerId, UserId, TopicId)
import Model.User (UserRef, Key(UserCoordKey), userRefServer, userRefUser)
import Model.StorableJson (StorableJson, storableEmpty)


data TopicMode = TopicMode {
    readable :: Bool,
    writable :: Bool,
    muted :: Bool,
    membersOnly :: Bool,
    authenticatedOnly :: Bool
} deriving (Show, Eq)

defaultTopicMode :: TopicMode
defaultTopicMode = TopicMode True True False True True

aboutTopicMode :: TopicMode
aboutTopicMode = TopicMode True False False True True

inviteTopicMode :: TopicMode
inviteTopicMode = TopicMode False True False True True

$(deriveJSON defaultOptions ''TopicMode)

makeLensesWith (lensRules & lensField .~ const lensName) ''TopicMode

data TopicCreate = TopicCreate {
    createId :: Maybe TopicId,
    createBanner :: Maybe T.Text,
    createInfo :: Maybe StorableJson,
    createMode :: Maybe TopicMode
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 6} ''TopicCreate)

data Topic = Topic {
    topicServer :: ServerId,
    topicUser :: UserId,
    topicId :: TopicId,
    topicBanner :: T.Text,
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

data TopicModeUpdate = TopicModeUpdate {
    updateReadable :: Maybe Bool,
    updateWritable :: Maybe Bool,
    updateMuted :: Maybe Bool,
    updateMembersOnly :: Maybe Bool,
    updateAuthenticatedOnly :: Maybe Bool
} deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 6 } ''TopicModeUpdate)

makeLensesWith (lensRules & lensField .~ const lensName)  ''TopicModeUpdate

resolveTopicModeUpdate :: TopicMode -> TopicModeUpdate -> TopicMode
resolveTopicModeUpdate tm tmu = snd $ resolveTopicModeUpdate' tm tmu

resolveTopicModeUpdate' :: TopicMode -> TopicModeUpdate -> (Bool, TopicMode) 
resolveTopicModeUpdate' = runUpdateR $ do
    readableLens ^.=? updateReadableLens
    writableLens ^.=? updateWritableLens
    mutedLens ^.=? updateMutedLens
    membersOnlyLens ^.=? updateMembersOnlyLens
    authenticatedOnlyLens ^.=? updateAuthenticatedOnlyLens

resolveTopicModeUpdateMay :: TopicMode -> TopicModeUpdate -> Maybe TopicMode
resolveTopicModeUpdateMay = asMaybe .* resolveTopicModeUpdate'
