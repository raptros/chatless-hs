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
    caller <- getCaller
    create <- requireJsonBody
    createTopic caller create >>= respondOpResult

eitherConst :: c -> c -> Either a b -> c
eitherConst l r = either (const l) (const r)

getLocalUserR :: UserId -> Handler Value
getLocalUserR uid = toJSON <$> getLocalUser uid

getLocalUserTopicsR :: UserId -> Handler Value
getLocalUserTopicsR uid = reader (refLocalUser uid) >>= listTopics >>= returnJson

listTopics :: (HasConn m cm conn, PersistBackend (DbPersist conn m)) => UserRef -> m [TopicRef]
listTopics = runDb . listTopicsOp

putSubsLocalR :: UserId -> TopicId -> Handler Value
putSubsLocalR uid tid = do
    caller <- getCaller
    let tr = TopicCoordKey (userRefServer caller) uid tid
    joinTopic caller tr >>= respondOpResult
    
