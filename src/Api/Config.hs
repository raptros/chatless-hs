{-# LANGUAGE TemplateHaskell #-}
module Api.Config where

import Chatless.Model.ID
import Control.Lens (makeLenses)
import Data.Pool (Pool)
import Database.Groundhog.Sqlite

type CLDb = Sqlite

data CLConfig = CLConfig {
    _clcServerId  :: ServerId,
    _clcDb :: Pool CLDb
}

makeLenses ''CLConfig
