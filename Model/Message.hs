{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.Message where

import Data.Text hiding (drop)
import qualified Data.Char as C

import Data.Aeson
import Data.Aeson.TH

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.TH

import Model.User
import Model.Topic
import Model.Utils
import Model.TopicMember
import Model.ID
import Model.StorableJson

import Control.Applicative

data MessageCore = MessageCore {
    msgcServer :: ServerId,
    msgcUser :: UserId,
    msgcTopic :: TopicId,
    msgcId :: MessageId,
    msgcCaller :: UserRef
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 4 } ''MessageCore)


data DbMessageType = 
    TypePostedMsg | 
    TypeUserJoinedMsg | 
    TypeSetMemberModeMsg | 
    TypeSetTopicModeMsg |
    TypeSetBannerMsg |
    TypeSetInfoMsg |
    TypeInvitationMsg |
    TypeSentInviteMsg
    deriving (Eq, Show, Enum)

data MessageIndexed = MessageIndexed {
    msgIdxServer :: ServerId,
    msgIdxUser :: UserId,
    msgIdxTopic :: TopicId,
    msgIdxId :: MessageId,
    --todo: timestamp
    msgIdxCaller :: UserRef,
    msgIdxType :: DbMessageType
}

mkPersist defaultCodegenConfig [groundhog|
- primitive: DbMessageType
  representation: enum
- entity: MessageIndexed
  autoKey:
    default: false
  keys:
    - name: MessageCoord
      default: true
  constructors:
    - name: MessageIndexed
      uniques:
        - name: MessageCoord
          type: index
          fields: [msgIdxServer, msgIdxUser, msgIdxTopic, msgIdxId]
      fields:
        - name: msgIdxServer
          dbName: server
        - name: msgIdxUser
          dbName: user
        - name: msgIdxTopic
          dbName: topic
        - name: msgIdxId
          dbName: id
        - name: msgIdxCaller
          dbName: server
        - name: msgIdxType
          dbName: server
|]

type MessageIdxRef = Key MessageIndexed (Unique MessageCoord)

data PostedMessage = PostedMessage {
    postedRef :: MessageIdxRef,
    postedBody :: StorableJson
}


