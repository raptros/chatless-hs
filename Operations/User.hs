{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.User where

import Operations.Base

import Model.User
import Model.Topic
import Database.Groundhog


listTopics :: CatchDbConn m cm conn => UserRef -> m (Either OpError [TopicRef])
listTopics ur = runOp $ project TopicCoord $ (TopicServerField ==. userRefServer ur) &&. (TopicUserField ==. userRefUser ur)

loadUser :: CatchDbConn m cm conn => (UserRef -> OpError) -> UserRef -> m (Either OpError User)
loadUser err ur = runOp $ getBy ur >>= getOrThrow (err ur)
