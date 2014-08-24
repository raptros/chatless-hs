{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Api where

import Yesod.Core (YesodSubDispatch, yesodSubDispatch)
import Yesod.Core.Handler (HandlerT)
import Yesod.Core.Dispatch (mkYesodDispatch, mkYesodSubDispatch)

import Api.Root 
import Api.Handlers
import Api.TopicSub.Data
import Api.TopicSub.Handlers


instance YesodSubDispatch TopicSub (HandlerT Chatless IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesTopicSub)

mkYesodDispatch "Chatless" resourcesChatless
