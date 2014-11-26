{-# LANGUAGE TemplateHaskell #-}
module Chatless.Model.ID  where

import Chatless.Model.IDGen

import Data.Text (Text)
import qualified System.Random as R
import Data.Aeson()
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Web.PathPieces (PathPiece, toPathPiece, fromPathPiece)
import Database.Groundhog () -- this is here just to silence :GhcModCheck
import Database.Groundhog.Core (PersistField(..), PrimitivePersistField(..), DbType(..), DbTypePrimitive'(..))
import Control.Lens ((%~), _1)
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)

newtype ServerId = ServerId Text deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''ServerId)

instance PathPiece ServerId where
    toPathPiece (ServerId i) = i
    fromPathPiece = Just . ServerId

instance PersistField ServerId where
    persistName _ = "ServerId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField ServerId where
    toPrimitivePersistValue p (ServerId sid) = toPrimitivePersistValue p sid
    fromPrimitivePersistValue p x = ServerId $ fromPrimitivePersistValue p x

newtype UserId = UserId Text deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''UserId)

instance PathPiece UserId where
    toPathPiece (UserId i) = i
    fromPathPiece = Just . UserId
    
instance PersistField UserId where
    persistName _ = "UserId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField UserId where
    toPrimitivePersistValue p (UserId uid) = toPrimitivePersistValue p uid
    fromPrimitivePersistValue p x = UserId $ fromPrimitivePersistValue p x

newtype TopicId = TopicId Text deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''TopicId)

instance PathPiece TopicId where
    toPathPiece (TopicId i) = i
    fromPathPiece = Just . TopicId

instance PersistField TopicId where
    persistName _ = "TopicId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField TopicId where
    toPrimitivePersistValue p (TopicId tid) = toPrimitivePersistValue p tid
    fromPrimitivePersistValue p x = TopicId $ fromPrimitivePersistValue p x

instance R.Random TopicId where
    random = (_1 %~ TopicId . unUUIDText) . R.random
    randomR _ = R.random

newtype MessageId = MessageId Text deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''MessageId)

instance PathPiece MessageId where
    toPathPiece (MessageId i) = i
    fromPathPiece = Just . MessageId

instance PersistField MessageId where
    persistName _ = "MessageId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField MessageId where
    toPrimitivePersistValue p (MessageId mid) = toPrimitivePersistValue p mid
    fromPrimitivePersistValue p x = MessageId $ fromPrimitivePersistValue p x

instance R.Random MessageId where
    random = (_1 %~ MessageId . unUUIDText) . R.random
    randomR _ = R.random

