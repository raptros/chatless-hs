{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Model.IDGen where
import Data.Text
import Data.Aeson
import Data.Aeson.TH
import Web.PathPieces
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.Random
import Control.Lens
import Model.ID
import Control.Monad.IO.Class


newtype UUIDText = UUIDText { unUUIDText :: T.Text } deriving (Eq, Show)

instance Random UUIDText where
    random = (_1 %~ UUIDText . pack . UUID.toString) . random
    randomR _ = random

instance Random TopicId where
    random = (_1 %~ TopicId . unUUIDText) . random
    randomR _ = random

instance Random MessageId where
    random = (_1 %~ MessageId . unUUIDText) . random
    randomR _ = random

genRandom :: (Random a, MonadIO m) => m a
genRandom = liftIO randomIO
