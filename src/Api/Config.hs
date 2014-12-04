{-# LANGUAGE TemplateHaskell #-}
module Api.Config where

import Chatless.Model.ID
import Control.Lens (makeLenses)
import Data.Pool (Pool)
import Database.Groundhog.Sqlite
import System.Log.FastLogger
import Control.Monad.Logger

type CLDb = Sqlite

data CLConfig = CLConfig {
    _clcServerId  :: ServerId,
    _clcDb :: Pool CLDb,
    _clcLogSet :: LoggerSet,
    _clcLogLevels :: LogLevel -> Bool
}



makeLenses ''CLConfig
