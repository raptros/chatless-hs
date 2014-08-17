{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
module Api.Handlers where

import Yesod.Core
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Generic (runDb, HasConn)
import Control.Monad.Reader
import Control.Applicative
import Data.Maybe
import Network.HTTP.Types
import Data.Text.Encoding (decodeUtf8)

import Model.ID
import Model.User
import Model.Topic
import Model.TopicMember
import Model.StorableJson
import Operations
import Api.Utils
import Api.Root
import Api.RootUtils

getMeR :: Handler Value
getMeR = do
    meRef <- getCaller
    me <- runDb $ getBy meRef
    maybe (meNotPresent meRef) returnJson me

getMeTopicsR :: Handler Value
getMeTopicsR = getCaller >>= listTopics >>= returnJson

postMeTopicsR :: Handler Value
postMeTopicsR = do
    sid <- reader localServer
    cid <- extractUserId
    create <- requireJsonBody :: Handler TopicCreate
    --todo random topic ids
    tid <- maybe (sendResponseStatus status400 $ reasonObject "id_required" []) return (createId create)
    let newTopic = createTopic sid cid tid create
        firstMember = Member (TopicCoordKey sid cid tid) (UserCoordKey sid cid) modeCreator
    --create a topic and insert the creator
    r <- runDb $ do
        tcRes <- insertByAll newTopic
        --if topic insertion did not fail constraints, neither should first member insertion
        insertByAll firstMember
        return tcRes
    eitherConst (sendResponseStatus status400 $ reasonObject "id_in_use" []) (returnJson newTopic) r

{-postMeTopicsR' :: Handler Value
postMeTopicsR' = do
    caller <- getCaller
    creation <- requireJsonBody :: Handler TopicCreate
    res <- createTopicOp caller creation-}
    

eitherConst :: c -> c -> Either a b -> c
eitherConst l r = either (const l) (const r)

createTopic :: ServerId -> UserId -> TopicId -> TopicCreate -> Topic
createTopic sid uid tid (TopicCreate _ mBanner mInfo mMode) = Topic {
    topicServer = sid,
    topicUser = uid,
    topicId = tid,
    topicBanner = fromMaybe "" mBanner,
    topicInfo = fromMaybe storableEmpty mInfo,
    topicMode = fromMaybe defaultTopicMode mMode
}



getLocalUserR :: UserId -> Handler Value
getLocalUserR uid = toJSON <$> getLocalUser uid

getLocalUserTopicsR :: UserId -> Handler Value
getLocalUserTopicsR uid = reader (refLocalUser uid) >>= listTopics >>= returnJson

listTopics :: (HasConn m cm conn, PersistBackend (DbPersist conn m)) => UserRef -> m [TopicRef]
listTopics = runDb . listTopicsOp

