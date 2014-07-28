{-# LANGUAGE FlexibleInstances, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.User where
import Database.Groundhog
import Database.Groundhog.TH
import Data.Aeson
import Data.Aeson.TH
import Model.ID
import Model.Utils
import Control.Applicative

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

instance ToJSON (Key User (Unique UserCoord)) where
    toJSON (UserCoordKey sid uid) = object ["server" .= sid, "user" .= uid]

instance FromJSON (Key User (Unique UserCoord)) where
    parseJSON = withObject "UserCoordKey" $ \v -> UserCoordKey <$> v .: "server" <*> v .: "user"
