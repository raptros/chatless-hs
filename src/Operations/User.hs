{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.User where

import Operations.Base (CatchDbConn, runOp, OpError, getOrThrow)

import Model.User (UserRef, userRefServer, userRefUser, User)
import Model.Topic (TopicRef, TopicCoord(..), Field(TopicServerField, TopicUserField))
import qualified Database.Groundhog as Gh


listTopics :: CatchDbConn m cm conn => UserRef -> m (Either OpError [TopicRef])
listTopics ur = runOp $ Gh.project TopicCoord $ (TopicServerField Gh.==. userRefServer ur) Gh.&&. (TopicUserField Gh.==. userRefUser ur)

loadUser :: CatchDbConn m cm conn => (UserRef -> OpError) -> UserRef -> m (Either OpError User)
loadUser err ur = runOp $ Gh.getBy ur >>= getOrThrow (err ur)
