{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
module Api.Handlers where

import Data.Aeson (Value)
import Control.Monad.Reader (reader)
import Yesod.Core.Json (requireJsonBody)

import Model.ID (ServerId, UserId, TopicId)
import Model.Topic (fromUserRef)

import qualified Operations as Op

import Api.Utils (respondOpResult)
import Api.Root (Handler)
import Api.RootUtils (getCaller, refLocalUser)

getMeR :: Handler Value
getMeR = getCaller >>= Op.loadUser Op.MeNotFound >>= respondOpResult

getMeTopicsR :: Handler Value
getMeTopicsR = getCaller >>= Op.listTopics >>= respondOpResult

postMeTopicsR :: Handler Value
postMeTopicsR = do
    caller <- getCaller
    create <- requireJsonBody
    Op.createTopic caller create >>= respondOpResult

getLocalUserR :: UserId -> Handler Value
getLocalUserR uid = reader (refLocalUser uid) >>= Op.loadUser Op.UserNotFound >>= respondOpResult

getLocalUserTopicsR :: UserId -> Handler Value
getLocalUserTopicsR uid = reader (refLocalUser uid) >>= Op.listTopics >>= respondOpResult

putSubsLocalR :: UserId -> TopicId -> Handler Value
putSubsLocalR uid tid = do
    caller <- getCaller
    localUser <- reader (refLocalUser uid)
    Op.joinTopic caller (fromUserRef tid localUser) >>= respondOpResult
    
