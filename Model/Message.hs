{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.Message where

import Data.Text hiding (drop)
import qualified Data.Char as C

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Pair, Parser, typeMismatch)

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.TH

import Model.User
import Model.Topic
import Model.Utils
import Model.TopicMember
import Model.ID
import Model.StorableJson

import qualified Data.HashMap.Strict as H

import Control.Applicative
import Control.Monad
import Data.Monoid

data MsgContent = 
    MsgPosted { mcBody :: StorableJson } |
    MsgBannerChanged { mcBanner :: Text } | 
    MsgUserJoined { mcMemberMode :: MemberMode } | 
    MsgTopicModeChanged { mcTopicMode :: TopicMode } | 
    MsgMemberModeChanged { mcMember :: UserRef, mcMemberMode :: MemberMode } |
    MsgInvitation { mcJoin :: TopicRef, mcMemberMode :: MemberMode, mcBody :: StorableJson } |
    MsgInvitedUser { mcMember :: UserRef, mcFromTopic :: TopicRef, mcMemberMode :: MemberMode }

msgContentFields :: MsgContent -> [Pair]
msgContentFields (MsgPosted body) = ["body" .= body]
msgContentFields (MsgBannerChanged banner) = ["banner" .= banner]
msgContentFields (MsgUserJoined mode) = ["mode" .= mode] -- note that we are using "mode"
msgContentFields (MsgTopicModeChanged mode) = ["mode" .= mode] -- for both Topic Mode and User Mode.
msgContentFields (MsgMemberModeChanged member mode) = ["user" .= member, "mode" .= mode]
msgContentFields (MsgInvitation join mode body) = ["join" .= join, "mode" .= mode, "body" .= body]
msgContentFields (MsgInvitedUser member fromTopic mode) = ["user" .= member, "from" .= fromTopic, "mode" .= mode ]

msgContentObject :: MsgContent -> Object
msgContentObject = H.fromList . msgContentFields

instance ToJSON MsgContent where
    toJSON = Object . msgContentObject

-- parsing a message content object is, unsurprisingly, complex.
msgContentFromObject :: Object -> Parser MsgContent
msgContentFromObject o 
    | hasUser && hasFrom && hasMode = MsgInvitedUser <$> o .: "user" <*> o .: "from" <*> o .: "mode"
    | hasJoin && hasMode && hasBody = MsgInvitation <$> o .: "join" <*> o .: "mode" <*> o .: "body"
    | hasUser && hasMode = MsgMemberModeChanged <$> o .: "user" <*> o .: "mode"
    | hasMode = (MsgTopicModeChanged <$> o .: "mode") <|> 
                (MsgUserJoined <$> o .: "mode") <|> 
                (typeMismatch "TopicMode or MemberMode" $ o H.! "mode") -- it is known that o contains "mode".
    | hasBanner = MsgBannerChanged <$> o .: "banner"
    | hasBody = MsgPosted <$> o .: "body"
    | otherwise = typeMismatch "MsgContent" $ Object o
    where hasField = (flip H.member) o
          hasBody = hasField "body"
          hasBanner = hasField "banner"
          hasMode = hasField "mode"
          hasJoin = hasField "join"
          hasFrom = hasField "from"
          hasUser = hasField "user"

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
msgRefObject (MessageCoordKey tr id) = (uncurry H.insert) ("message" .= id) (topicRefObject tr) 

instance ToJSON MessageRef where
    toJSON = Object . msgRefObject

instance FromJSON MessageRef where
    parseJSON = withObject "MessageCoordKey" $ \v -> MessageCoordKey <$> (parseJSON $ Object v) <*> (v .: "message")

data Message = Message {
    msgTopic :: TopicRef,
    msgId :: MessageId,
    msgSender :: UserRef,
    msgData :: MsgContent
}

messageObject :: Message -> Object
messageObject m = (topicRefObject (msgTopic m)) <> messageObjectParts <> (msgContentObject (msgData m))
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
handleFromMessage k (Message tr id sender _) = MsgHandle tr id sender k

