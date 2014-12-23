{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Chatless.Model.Topic where

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

import Chatless.Model.Utils (mkLensName, dropAndLowerHead, (^.=?), runUpdateR, asMaybe)
import Chatless.Model.ID (ServerId, UserId, TopicId)
import Chatless.Model.User (UserRef, Key(UserCoordKey), userRefServer, userRefUser, userServer, userId, userAbout, userInvite, User)
import Chatless.Model.StorableJson (StorableJson, storableEmpty)
import Chatless.Model.Topic (TopicRef)


data Subscription = Subscription {
    subscriptionOwner :: UserRef,
    subscriptionId :: SubscriptionId
    subscriptionTopic :: TopicRef,
    subscriptionActive :: Bool
}

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 12 } ''Subscription)

mkPersist defaultCodegenConfig [groundhog|
- entity: Subscription
  autoKey: null
  keys:
    - name: SubscriptionCoord
      default: true
    - name: SubscriptionTarget
      default: false
  constructors:
    - name: Subscription
      uniques:
        - name: SubscriptionCoord
          type: primary
          fields: [subscriptionOwner, subscriptionId]
        - name: SubscriptionTarget
          type: index
          fields: [subscriptionOwner, subscriptionTopic]
|]

