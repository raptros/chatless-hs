{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Api.TopicSub.Handlers where

import Api.Utils
import Api.TopicSub.Data
import Api.Root
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
import Control.Applicative

type TopicHandler a = HandlerT TopicSub Handler a
--type ChatlessTopicHandler a = TopicHandler Chatless a

getTopicR :: TopicHandler Value
getTopicR = do
    tc <- readTopicCoord
    mTopic <- lift $ runDb $ getBy tc
    maybe notFound returnJson mTopic


--data TopicRef = TopicRef ServerId UserId TopicId

type TopicRef = Key Topic (Unique TopicCoord)

readTopicCoord :: TopicHandler TopicRef
readTopicCoord = ask >>= getTopicCoord

getTopicCoord :: TopicSub -> TopicHandler TopicRef
getTopicCoord (MeTopicSub sid tid) = (\uid -> TopicCoordKey sid uid tid) <$> extractUserId
getTopicCoord (MeAboutTopicSub sid) = pickTopicFromUser userAbout sid <$> loadMe sid
getTopicCoord (MeInvitesTopicSub sid) = pickTopicFromUser userInvite sid <$> loadMe sid


pickTopicFromUser :: (User -> TopicId) -> ServerId -> User -> TopicRef
pickTopicFromUser c sid u = TopicCoordKey sid (userId u) (c u)

--getTopic = do
    --
loadMe :: ServerId -> TopicHandler User
loadMe sid = do
    uid <- extractUserId
    mUser <- lift $ runDb $ getBy $ UserCoordKey sid uid
    maybe (meNotPresent' sid uid) return mUser


meNotPresent' :: ServerId -> UserId -> TopicHandler a
meNotPresent' sid uid = sendResponseStatus status500 $ reasonObject "me_not_present" ["server" .= sid, "user" .= uid]


{-
getLocalUserTopicR :: UserId -> TopicId -> Handler Value
getLocalUserTopicR uid tid = do
    lserv <- reader localServer
    mTopic <- runDb $ getBy $ TopicCoordKey lserv uid tid
    maybe notFound returnJson mTopic
    -}
