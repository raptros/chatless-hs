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

data TopicSub = MeTopicSub TopicId |
                MeAboutTopicSub |
                MeInvitesTopicSub |
                LocalUserTopicSub UserId TopicId |
                LocalUserAboutTopicSub UserId |
                LocalUserInvitesTopicSub UserId |
                AnyUserAboutTopicSub ServerId UserId |
                AnyUserInvitesTopicSub ServerId UserId |
                AnyUserTopicSub ServerId UserId TopicId


mkYesodSubData "TopicSub" [parseRoutes|
/ TopicR GET
/member MembersR GET
/member/user/#UserId LocalMemberR GET PUT POST
/member/server/#ServerId/user/#UserId MemberR GET PUT POST
|]
