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
getMeR = getCaller >>= loadUser MeNotFound >>= respondOpResult

getMeTopicsR :: Handler Value
getMeTopicsR = getCaller >>= listTopics >>= respondOpResult

postMeTopicsR :: Handler Value
postMeTopicsR = do
    caller <- getCaller
    create <- requireJsonBody
    createTopic caller create >>= respondOpResult
--postMeTopicsR = hold2 <$> getCaller <*> requireJsonBody >>= (createTopic &) >>= respondOpResult

getLocalUserR :: UserId -> Handler Value
getLocalUserR uid = reader (refLocalUser uid) >>= loadUser UserNotFound >>= respondOpResult

getLocalUserTopicsR :: UserId -> Handler Value
getLocalUserTopicsR uid = reader (refLocalUser uid) >>= listTopics >>= respondOpResult

putSubsLocalR :: UserId -> TopicId -> Handler Value
putSubsLocalR uid tid = do
    caller <- getCaller
    let tr = TopicCoordKey (userRefServer caller) uid tid
    joinTopic caller tr >>= respondOpResult
    
