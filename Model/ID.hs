{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Model.ID where
import Data.Text
import Data.Aeson
import Data.Aeson.TH
import Web.PathPieces
import Database.Groundhog.Core
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)

newtype ServerId = ServerId Text deriving (Eq, Show, Read)

$(deriveJSON defaultOptions ''ServerId)

instance PathPiece ServerId where
    toPathPiece (ServerId i) = i
    fromPathPiece = Just . ServerId

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

newtype MessageId = MessageId Text deriving (Eq, Show, Read)

instance PathPiece MessageId where
    toPathPiece (MessageId i) = i
    fromPathPiece = Just . MessageId

$(deriveJSON defaultOptions ''MessageId)

instance PersistField ServerId where
    persistName _ = "ServerId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField ServerId where
    toPrimitivePersistValue p (ServerId sid) = toPrimitivePersistValue p sid
    fromPrimitivePersistValue p x = ServerId $ fromPrimitivePersistValue p x

instance PersistField UserId where
    persistName _ = "UserId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField UserId where
    toPrimitivePersistValue p (UserId uid) = toPrimitivePersistValue p uid
    fromPrimitivePersistValue p x = UserId $ fromPrimitivePersistValue p x

instance PersistField TopicId where
    persistName _ = "TopicId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField TopicId where
    toPrimitivePersistValue p (TopicId tid) = toPrimitivePersistValue p tid
    fromPrimitivePersistValue p x = TopicId $ fromPrimitivePersistValue p x

instance PersistField MessageId where
    persistName _ = "MessageId"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField MessageId where
    toPrimitivePersistValue p (MessageId mid) = toPrimitivePersistValue p mid
    fromPrimitivePersistValue p x = MessageId $ fromPrimitivePersistValue p x
