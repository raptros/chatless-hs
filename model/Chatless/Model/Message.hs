{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Chatless.Model.Message where

import Data.Text (Text)
import qualified Data.HashMap.Strict as H
import Data.Aeson (ToJSON, FromJSON, (.=), (.:), Object, Value(..), withObject, parseJSON, toJSON)
import Data.Aeson.Types (Pair, Parser, typeMismatch)
import Data.Monoid ((<>))
import Control.Applicative ((<$>), (<*>), (<|>))

import Database.Groundhog (DefaultKey, PersistBackend, Unique, AutoKey, migrate, Key)
import Database.Groundhog.Core (Migration)
import Database.Groundhog.TH (mkPersist, defaultCodegenConfig, groundhog, namingStyle, persistentNamingStyle)

import Chatless.Model.StorableJson (StorableJson)
import Chatless.Model.ID (MessageId)
import Chatless.Model.User (UserRef)
import Chatless.Model.Topic (TopicMode, TopicRef, topicRefObject, topicRefFromObject)
import Chatless.Model.TopicMember (MemberMode)

data MsgContent = 
    MsgPosted { mcBody :: StorableJson } |
    MsgBannerChanged { mcBanner :: Text } | 
    MsgInfoChanged { mcInfo :: StorableJson } |
    MsgUserJoined { mcMemberMode :: MemberMode } | 
    MsgTopicModeChanged { mcTopicMode :: TopicMode } | 
    MsgMemberModeChanged { mcMember :: UserRef, mcMemberMode :: MemberMode } |
    MsgInvitation { mcJoin :: TopicRef, mcMemberMode :: MemberMode, mcBody :: StorableJson } |
    MsgInvitedUser { mcMember :: UserRef, mcFromTopic :: TopicRef, mcMemberMode :: MemberMode }

msgContentFields :: MsgContent -> [Pair]
msgContentFields (MsgPosted body) = ["body" .= body]
msgContentFields (MsgBannerChanged banner) = ["banner" .= banner]
msgContentFields (MsgInfoChanged info) = ["info" .= info]
msgContentFields (MsgUserJoined mode) = ["mode" .= mode] -- note that we are using "mode"
msgContentFields (MsgTopicModeChanged mode) = ["mode" .= mode] -- for both Topic Mode and User Mode.
msgContentFields (MsgMemberModeChanged member mode) = ["member" .= member, "mode" .= mode]
msgContentFields (MsgInvitation tr mode body) = ["join" .= tr, "mode" .= mode, "body" .= body]
msgContentFields (MsgInvitedUser member fromTopic mode) = ["member" .= member, "from" .= fromTopic, "mode" .= mode ]

msgContentObject :: MsgContent -> Object
msgContentObject = H.fromList . msgContentFields

instance ToJSON MsgContent where
    toJSON = Object . msgContentObject

-- parsing a message content object is, unsurprisingly, complex.
msgContentFromObject :: Object -> Parser MsgContent
msgContentFromObject o 
    | hasMember && hasFrom && hasMode = MsgInvitedUser <$> o .: "member" <*> o .: "from" <*> o .: "mode"
    | hasJoin && hasMode && hasBody = MsgInvitation <$> o .: "join" <*> o .: "mode" <*> o .: "body"
    | hasMember && hasMode = MsgMemberModeChanged <$> o .: "member" <*> o .: "mode"
    | hasMode = (MsgTopicModeChanged <$> o .: "mode") <|> 
                (MsgUserJoined <$> o .: "mode") <|> 
                typeMismatch "TopicMode or MemberMode" (o H.! "mode") -- it is known that o contains "mode".
    | hasBanner = MsgBannerChanged <$> o .: "banner"
    | hasInfo = MsgInfoChanged <$> o .: "info"
    | hasBody = MsgPosted <$> o .: "body"
    | otherwise = typeMismatch "MsgContent" $ Object o
    where hasField = flip H.member o
          hasBody = hasField "body"
          hasBanner = hasField "banner"
          hasInfo = hasField "info"
          hasMode = hasField "mode"
          hasJoin = hasField "join"
          hasFrom = hasField "from"
          hasMember  = hasField "member"

instance FromJSON MsgContent where
    parseJSON = withObject "MsgContent" msgContentFromObject

mkPersist (defaultCodegenConfig { namingStyle = persistentNamingStyle }) [groundhog|
- entity: MsgContent
  autoKey:
    default: true
|]

data MsgHandle = MsgHandle {
    mhTopic :: TopicRef,
    mhId :: MessageId,
    mhCaller :: UserRef,
    mhContent :: DefaultKey MsgContent
}

mkPersist defaultCodegenConfig [groundhog|
- entity: MsgHandle
  autoKey:
    default: false
  keys:
    - name: MessageCoord
      default: true
  constructors:
    - name: MsgHandle
      uniques:
        - name: MessageCoord
          type: index
          fields: [mhTopic, mhId]
|]

messageMigration :: PersistBackend m => Migration m
messageMigration = do
    migrate (undefined :: MsgContent)
    migrate (undefined :: MsgHandle)

type MessageRef = Key MsgHandle (Unique MessageCoord)

msgRefObject :: MessageRef -> Object
msgRefObject (MessageCoordKey tr mid) = uncurry H.insert ("message" .= mid) (topicRefObject tr) 

instance ToJSON MessageRef where
    toJSON = Object . msgRefObject

instance FromJSON MessageRef where
    parseJSON = withObject "MessageCoordKey" $ \v -> MessageCoordKey <$> parseJSON (Object v) <*> (v .: "message")

data Message = Message {
    msgTopic :: TopicRef,
    msgId :: MessageId,
    msgSender :: UserRef,
    msgData :: MsgContent
}

messageObject :: Message -> Object
messageObject m = topicRefObject (msgTopic m) <> messageObjectParts <> msgContentObject (msgData m)
  where messageObjectParts = H.fromList ["id" .= msgId m, "sender" .= msgSender m]

instance ToJSON Message where
    toJSON = Object . messageObject 

instance FromJSON Message where
    parseJSON = withObject "Message" $ \o -> 
                Message <$> 
                topicRefFromObject o <*> 
                o .: "id" <*> 
                o .: "sender" <*> 
                msgContentFromObject o

handleFromMessage :: AutoKey MsgContent -> Message -> MsgHandle
handleFromMessage k (Message tr mid sender _) = MsgHandle tr mid sender k

