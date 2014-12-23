{-# LANGUAGE TemplateHaskell #-}
module Chatless.Model.ID  where

import qualified Data.Text as T
import qualified System.Random as R
import qualified Data.UUID as UUID
import Data.Aeson()
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Web.PathPieces (PathPiece, toPathPiece, fromPathPiece)
import Database.Groundhog () -- this is here just to silence :GhcModCheck
import Database.Groundhog.Core (PersistField(..), PrimitivePersistField(..), DbType(..), DbTypePrimitive'(..))
import Control.Lens ((%~), _1)
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)

-- * stuff for generating randoms

newtype UUIDText = UUIDText { unUUIDText :: T.Text } deriving (Eq, Show)

instance R.Random UUIDText where
    random = (_1 %~ UUIDText . T.pack . UUID.toString) . R.random
    randomR _ = R.random

-- * the id types 

-- ** server id

newtype ServerId = ServerId T.Text deriving (Eq, Show, Read)

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

-- ** user id

newtype UserId = UserId T.Text deriving (Eq, Show, Read)

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

-- ** topic id

newtype TopicId = TopicId T.Text deriving (Eq, Show, Read)

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

-- ** message id

newtype MessageId = MessageId T.Text deriving (Eq, Show, Read)

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

-- * subscription id

newtype SubscriptionId = SubscriptionId T.Text deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''SubscriptionId)

instance PathPiece SubscriptionId where
    toPathPiece (SubscriptionId i) = i
    fromPathMultiPiece = Just . SubscriptionId

instance PersistField SubscriptionId where
    persistName _ = "SubscriptionId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField SubscriptionId where
    toPrimitivePersistValue p (SubscriptionId sid) = toPrimitivePersistValue p sid
    fromPrimitivePersistValue p x = SubscriptionId $ fromPrimitivePersistValue p x
