{-# LANGUAGE FlexibleInstances, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.User (User(..), UserRef, Key(UserCoordKey), userRefServer, userRefUser) where
import Database.Groundhog
import Database.Groundhog.TH
import Data.Aeson
import Data.Aeson.TH
import Model.ID
import Model.Utils
import Control.Applicative
import qualified Data.HashMap.Strict as H

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
