{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Model.ID where
import Data.Text
import Data.Aeson
import Data.Aeson.TH
import Web.PathPieces
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)

newtype ServerId = ServerId Text deriving (Eq, Show)

$(deriveJSON defaultOptions ''ServerId)

newtype UserId = UserId Text deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''UserId)

instance PathPiece UserId where
    toPathPiece (UserId i) = i
    fromPathPiece = Just . UserId
    
newtype TopicId = TopicId Text deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''TopicId)

instance PathPiece TopicId where
    toPathPiece (TopicId i) = i
    fromPathPiece = Just . TopicId

newtype MessageId = MessageId Text deriving (Eq, Show)

$(deriveJSON defaultOptions ''MessageId)

instance PersistField ServerId where
    persistName _ = "ServerId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField ServerId where
    toPrimitivePersistValue p (ServerId id) = toPrimitivePersistValue p id
    fromPrimitivePersistValue p x = ServerId $ fromPrimitivePersistValue p x

instance PersistField UserId where
    persistName _ = "UserId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField UserId where
    toPrimitivePersistValue p (UserId id) = toPrimitivePersistValue p id
    fromPrimitivePersistValue p x = UserId $ fromPrimitivePersistValue p x

instance PersistField TopicId where
    persistName _ = "TopicId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField TopicId where
    toPrimitivePersistValue p (TopicId id) = toPrimitivePersistValue p id
    fromPrimitivePersistValue p x = TopicId $ fromPrimitivePersistValue p x
