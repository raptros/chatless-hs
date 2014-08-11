{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs #-}
{- this exists so i can store json objects and arrays as primitives -}
module Model.StorableJson (StorableJson, storableObject, storableEmpty) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Control.Applicative
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic (primToPersistValue, primFromPersistValue)
import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Maybe (fromMaybe)

newtype StorableJson = StorableJson B.ByteString deriving (Show, Eq)

instance ToJSON StorableJson where
    toJSON (StorableJson s) = fromMaybe (object []) v
      where v = decodeStrict' s :: Maybe Value

parseToStorable :: Value -> Parser StorableJson
parseToStorable = pure . StorableJson . LB.toStrict . encode
    
instance FromJSON StorableJson where
    parseJSON v @ (Object _) = parseToStorable v
    parseJSON v @ (Array _) = parseToStorable v
    parseJSON v = typeMismatch "StorableJson" v

instance PersistField StorableJson where
    persistName _ = "StorableJson"
    toPersistValues = primToPersistValue
    fromPersistValues = primFromPersistValue
    dbType _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField StorableJson where
    toPrimitivePersistValue p (StorableJson bs) = toPrimitivePersistValue p bs
    fromPrimitivePersistValue p x = StorableJson $ fromPrimitivePersistValue p x

instance NeverNull StorableJson

storableObject :: Object -> StorableJson
storableObject = StorableJson . LB.toStrict . encode . Object

storableEmpty :: StorableJson
storableEmpty = StorableJson $ LB.toStrict $ encode $ object []
