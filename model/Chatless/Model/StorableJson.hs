{-|

'StorableJson' is a newtype wrapper around Json values that has instances for persisting it in a database.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
 
module Chatless.Model.StorableJson where 

import Control.Applicative (pure)
import Database.Groundhog ()
import qualified Database.Groundhog.Core as Gh
import qualified Database.Groundhog.Generic as Gh
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

newtype StorableJson = StorableJson { 
    storableJsonValue :: A.Value 
} deriving (Show, Eq)

instance A.ToJSON StorableJson where
    toJSON = storableJsonValue

instance A.FromJSON StorableJson where
    parseJSON = pure . StorableJson

instance Gh.PersistField StorableJson where
    persistName _ = "StorableJson"
    toPersistValues = Gh.primToPersistValue
    fromPersistValues = Gh.primFromPersistValue
    dbType _ _ = Gh.DbTypePrimitive Gh.DbString False Nothing Nothing

instance Gh.SinglePersistField StorableJson where
    toSinglePersistValue = Gh.primToSinglePersistValue
    fromSinglePersistValue = Gh.primFromSinglePersistValue

instance Gh.PurePersistField StorableJson where
    toPurePersistValues = Gh.primToPurePersistValues
    fromPurePersistValues = Gh.primFromPurePersistValues

instance Gh.PrimitivePersistField StorableJson where
    toPrimitivePersistValue p = Gh.toPrimitivePersistValue p . A.encode . storableJsonValue
    fromPrimitivePersistValue p = maybe storableEmpty StorableJson . A.decode . Gh.fromPrimitivePersistValue p

instance Gh.NeverNull StorableJson

storableEmpty :: StorableJson
storableEmpty = StorableJson A.emptyObject
