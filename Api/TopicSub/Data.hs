{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Api.TopicSub.Data where

import Api.Utils
import Yesod.Core

import Model.ID
import Model.User
import Model.Topic
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Generic (runDb)
import Database.Groundhog.Sqlite
import Database.Groundhog.Postgresql
import Database.Groundhog.Core (ConnectionManager(..))
import Control.Monad.Reader
import Network.HTTP.Types
import Data.Pool
import Data.Text.Encoding (decodeUtf8)

data TopicSub = MeTopicSub ServerId TopicId |
                MeAboutTopicSub ServerId |
                MeInvitesTopicSub ServerId |
                LocalUserTopicSub ServerId UserId TopicId |
                LocalUserAboutTopicSub ServerId UserId |
                LocalUserInvitesTopicSub ServerId UserId |
                AnyUserTopicSub ServerId UserId TopicId


mkYesodSubData "TopicSub" [parseRoutes|
/ TopicR GET
|]
