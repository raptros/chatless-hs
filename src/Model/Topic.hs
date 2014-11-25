{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, MultiParamTypeClasses, FunctionalDependencies #-}
module Model.Topic where

import Control.Applicative ((<$>), (<*>))

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

import Data.Aeson (ToJSON, FromJSON, (.=), (.:), toJSON, parseJSON, Object, Value(..), withObject, object)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Aeson.Types (Parser)

import Database.Groundhog.Core (Key, Unique)
import Database.Groundhog.TH (mkPersist, defaultCodegenConfig, groundhog)

import Control.Lens.TH (makeLensesWith, lensRules, lensField)
import Control.Lens.Operators ((&), (.~))

import Utils ((.*))

import Model.Utils (mkLensName, dropAndLowerHead, (^.=?), runUpdateR, asMaybe)
import Model.ID (ServerId, UserId, TopicId)
import Model.User (UserRef, Key(UserCoordKey), userRefServer, userRefUser, userServer, userId, userAbout, User)
import Model.StorableJson (StorableJson, storableEmpty)

-- * Topic data model!

-- ** Topic mode
-- | 
data TopicMode = TopicMode {
    readable :: Bool,
    writable :: Bool,
    muted :: Bool,
    membersOnly :: Bool,
    authenticatedOnly :: Bool
} deriving (Show, Eq)

$(deriveJSON defaultOptions ''TopicMode)

-- | readable, writable, not muted, members only, authenticated only
defaultTopicMode :: TopicMode
defaultTopicMode = TopicMode True True False True True

-- | mode for creating "about" topics - readable, not writeable, not muted,
-- members only, authenticated only
aboutTopicMode :: TopicMode
aboutTopicMode = TopicMode True False False True True

-- | mode for creating "invite" topics - not readable, writeable, not
-- muted, members only, authenticated only
inviteTopicMode :: TopicMode
inviteTopicMode = TopicMode False True False True True

-- *** lenses into topic mode
makeLensesWith (lensRules & lensField .~ mkLensName) ''TopicMode

-- *** updating the topic mode

-- | representation of a request to modify the topic mode
data TopicModeUpdate = TopicModeUpdate {
    updateReadable :: Maybe Bool,
    updateWritable :: Maybe Bool,
    updateMuted :: Maybe Bool,
    updateMembersOnly :: Maybe Bool,
    updateAuthenticatedOnly :: Maybe Bool
} deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 6 } ''TopicModeUpdate)

-- **** lenses into the topic mode update 
makeLensesWith (lensRules & lensField .~ mkLensName)  ''TopicModeUpdate

-- **** applying updates

-- | output is the whether or not any modifications were made to the topic
-- mode and the final state of the topic mode
resolveTopicModeUpdate :: TopicMode -> TopicModeUpdate -> (Bool, TopicMode) 
resolveTopicModeUpdate = runUpdateR $ do
    readableLens ^.=? updateReadableLens
    writableLens ^.=? updateWritableLens
    mutedLens ^.=? updateMutedLens
    membersOnlyLens ^.=? updateMembersOnlyLens
    authenticatedOnlyLens ^.=? updateAuthenticatedOnlyLens

-- | return a new state for the topic mode if any updates occurred.
resolveTopicModeUpdateMay :: TopicMode -> TopicModeUpdate -> Maybe TopicMode
resolveTopicModeUpdateMay = asMaybe .* resolveTopicModeUpdate

-- ** topic create object
data TopicCreate = TopicCreate {
    createId :: Maybe TopicId,
    createBanner :: Maybe T.Text,
    createInfo :: Maybe StorableJson,
    createMode :: Maybe TopicMode
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 6} ''TopicCreate)

-- | create a topic.
initializeTopic :: UserRef -> TopicId -> TopicCreate -> Topic
initializeTopic caller tid (TopicCreate _ mBanner mInfo mMode) = Topic {
    topicServer = userRefServer caller,
    topicUser = userRefUser caller,
    topicId = tid,
    topicBanner = fromMaybe "" mBanner,
    topicInfo = fromMaybe storableEmpty mInfo,
    topicMode = fromMaybe defaultTopicMode mMode
}

-- ** the topic itself
data Topic = Topic {
    topicServer :: ServerId,
    topicUser :: UserId,
    topicId :: TopicId,
    topicBanner :: T.Text,
    topicInfo :: StorableJson,
    topicMode :: TopicMode
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 5 } ''Topic)

-- *** generated database stuff
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

-- *** topic utils 
-- | whether or not there must be an authenticated caller in order to read
-- this topic
authRequired :: Topic -> Bool
authRequired = authenticatedOnly . topicMode

-- | whether or not a user must be a member in order to read this topic
membershipRequired :: Topic -> Bool
membershipRequired = membersOnly . topicMode

-- | did this user create this topic
isCreator :: UserRef -> Topic -> Bool
isCreator (UserCoordKey sid uid) t = (sid == topicServer t) && (uid == topicUser t)

isUserCreator :: User -> Topic -> Bool
isUserCreator user topic = (userServer user == topicServer topic) && (userId user == topicUser topic)

-- | the instance of ToJSON for CensoredTopic only includes the server,
-- user, and id fields of the wrapped topic.
newtype CensoredTopic = CensoredTopic Topic

instance ToJSON CensoredTopic where
    toJSON (CensoredTopic topic) = object ["server" .= topicServer topic, "user" .= topicUser topic, "id" .= topicId topic]

-- ** topic references

-- | reference key for a topic
type TopicRef = Key Topic (Unique TopicCoord)

-- *** obtaining topic references

fromUserRef :: TopicId -> UserRef -> TopicRef
fromUserRef tid (UserCoordKey sid uid) = TopicCoordKey sid uid tid

userTopicRef :: User -> TopicId -> TopicRef
userTopicRef user tid = TopicCoordKey (userServer user) (userId user) tid

topicRefFromUser :: TopicId -> User -> TopicRef
topicRefFromUser = flip userTopicRef

userAboutTopicRef :: User -> TopicRef
userAboutTopicRef user = TopicCoordKey (userServer user) (userId user) (userAbout user)

userInviteTopicRef :: User -> TopicRef
userInviteTopicRef user = TopicCoordKey (userServer user) (userId user) (userAbout user)

getRefFromTopic :: Topic -> TopicRef
getRefFromTopic = TopicCoordKey <$> topicServer <*> topicUser <*> topicId

-- *** getting things out of topic references

-- | get the user ref from the topic ref
topicRefUserRef :: TopicRef -> UserRef
topicRefUserRef (TopicCoordKey sid uid _) = UserCoordKey sid uid

-- |
topicRefId :: TopicRef -> TopicId
topicRefId (TopicCoordKey _ _ tid) = tid

-- *** using topic refs as JSON

topicRefObject :: TopicRef -> Object
topicRefObject (TopicCoordKey sid uid tid) = H.fromList ["server" .= sid, "user" .= uid, "topic" .= tid]

instance ToJSON (Key Topic (Unique TopicCoord)) where
    toJSON = Object . topicRefObject

topicRefFromObject :: Object -> Parser TopicRef
topicRefFromObject v = TopicCoordKey <$> v .: "server" <*> v .: "user" <*> v .: "topic"

instance FromJSON (Key Topic (Unique TopicCoord)) where
    parseJSON = withObject "TopicCoordKey" topicRefFromObject

