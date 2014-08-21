{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.User where

import Operations.Base

import Model.ID
import Model.User
import Model.Topic
import Model.TopicMember
import Model.StorableJson
import Model.Message
import Control.Monad
import Control.Monad.Except
import Control.Monad.Catch
import Database.Groundhog
import Database.Groundhog.Generic
import Control.Applicative
import Control.Monad.Random
import Data.Maybe
import Model.IDGen


listTopics :: CatchDbConn m cm conn => UserRef -> m (Either OpError [TopicRef])
listTopics ur = runOp $ project TopicCoord $ (TopicServerField ==. userRefServer ur) &&. (TopicUserField ==. userRefUser ur)

loadUser :: CatchDbConn m cm conn => (UserRef -> OpError) -> UserRef -> m (Either OpError User)
loadUser err ur = runOp $ getBy ur >>= getOrThrow (err ur)


