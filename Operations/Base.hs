{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Operations.Base where

import Model.ID
import Model.User
import Model.Topic
import Control.Monad.Except
import qualified Data.Text as T
import Control.Monad.Random

data ApiOperation = 
    ReadMembers |
    SetMemberMode

data ApiError = 
    MeNotFound UserRef |
    TopicNotFound TopicRef |
    UserNotFound UserRef |
    MemberNotFound TopicRef UserRef |
    OperationDenied ApiOperation |
    IdInUse TopicRef |
    GenerateIdFailed UserRef [TopicId]


getOrThrow :: (MonadError e m) => e -> Maybe a -> m a
getOrThrow err = maybe (throwError err) return


--randomId :: MonadRandom m => Int -> m T.Text
