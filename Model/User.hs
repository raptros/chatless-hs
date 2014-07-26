{-# LANGUAGE FlexibleInstances, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.User where
import Database.Groundhog
import Database.Groundhog.TH
import Data.Aeson
import Data.Aeson.TH
import Model.ID
import Model.Utils

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
