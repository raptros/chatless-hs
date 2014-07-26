{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Api.Handlers where

import Api.Utils
import Api.Root
import Yesod.Core

import Model.ID
import Model.User
import Model.Topic
import Model.StorableJson
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Generic (runDb)
import Control.Monad.Reader
import Data.Maybe
import Network.HTTP.Types
import Data.Text.Encoding (decodeUtf8)


getMeR :: Handler Value
getMeR = do
    lserv <- reader localServer
    cid <- extractUserId
    me <- runDb $ getBy $ UserCoordKey lserv cid
    maybe (meNotPresent lserv cid) returnJson me

getMeTopicsR :: Handler Value
getMeTopicsR = do
    lserv <- reader localServer
    cid <- extractUserId
    topics <- runDb $ select $ (TopicServerField ==. lserv) &&. (TopicUserField ==. cid)
    returnJson topics

postMeTopicsR :: Handler Value
postMeTopicsR = do
    sid <- reader localServer
    cid <- extractUserId
    create <- requireJsonBody :: Handler TopicCreate
    --todo random topic ids
    tid <- maybe (sendResponseStatus status400 $ reasonObject "id_required" []) return (createId create)
    let newTopic = createTopic sid cid tid create
    r <- runDb $ insertByAll newTopic
    either (const $ sendResponseStatus status400 $ reasonObject "id_in_use" []) (const $ returnJson newTopic) r

createTopic :: ServerId -> UserId -> TopicId -> TopicCreate -> Topic
createTopic sid uid tid (TopicCreate _ mBanner mInfo mMode) = Topic {
    topicServer = sid,
    topicUser = uid,
    topicId = tid,
    topicBanner = fromMaybe "" mBanner,
    topicInfo = fromMaybe storableEmpty mInfo,
    topicMode = fromMaybe defaultTopicMode mMode
}


meNotPresent :: ServerId -> UserId -> Handler Value
meNotPresent sid uid = sendResponseStatus status500 $ reasonObject "me_not_present" ["server" .= sid, "user" .= uid]

getLocalUserR :: UserId -> Handler Value
getLocalUserR uid = do
    lserv <- reader localServer
    mUser <- runDb $ getBy $ UserCoordKey lserv uid
    maybe notFound returnJson mUser

getLocalUserTopicsR :: UserId -> Handler Value
getLocalUserTopicsR id = do
    lserv <- reader localServer
    topics <- runDb $ select $ (TopicServerField ==. lserv) &&. (TopicUserField ==. id)
    returnJson topics
