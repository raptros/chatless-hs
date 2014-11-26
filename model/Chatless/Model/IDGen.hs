module Chatless.Model.IDGen where

import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.Random (Random, random, randomR, randomIO)
import Control.Lens ((%~), _1)
import Control.Monad.IO.Class (MonadIO, liftIO)

newtype UUIDText = UUIDText { unUUIDText :: T.Text } deriving (Eq, Show)

instance Random UUIDText where
    random = (_1 %~ UUIDText . T.pack . UUID.toString) . random
    randomR _ = random

genRandom :: (Random a, MonadIO m) => m a
genRandom = liftIO randomIO
