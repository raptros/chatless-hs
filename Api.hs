{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Api where

import Yesod.Core

import Api.Utils
import Api.Root
import Api.Handlers
import Api.TopicSub.Data
import Api.TopicSub.Handlers

import Model.ID
import Model.User
import Model.Topic
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Generic (runDb)
import Control.Monad.Reader
import Network.HTTP.Types
import Data.Pool
import Data.Text.Encoding (decodeUtf8)

instance YesodSubDispatch TopicSub (HandlerT Chatless IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesTopicSub)

mkYesodDispatch "Chatless" resourcesChatless
