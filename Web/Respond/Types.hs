{-|
Description: base types 

contains a couple basic types for Respond
-}
{-# LANGUAGE TemplateHaskell #-}
module Web.Respond.Types where

import Network.Wai
import qualified Data.Text as T
import Control.Monad.Trans.State
import qualified Data.Sequence as S

import Control.Lens (makeLenses, snoc, (%=), uses)
import Safe (headMay, tailSafe)

-- * responding

-- | instances of ToResponse can be converted into responses
class ToResponse a where
    -- | this is how
    toResponse :: a -> Response

instance ToResponse Response where
    toResponse = id

-- | the type of the responder callback that is handed to a WAI
-- 'Network.Wai.Application'
type Responder = Response -> IO ResponseReceived

-- * working with the path.

-- | stores the path and how much of it has been consumed
data PathConsumer = PathConsumer {
    -- | the consumed part of the path.
    _pcConsumed :: S.Seq T.Text,
    -- | the unconsumed part
    _pcUnconsumed :: [T.Text]
} deriving (Eq, Show)

makeLenses ''PathConsumer

-- | build a path consumer starting with nothing consumed
mkPathConsumer :: [T.Text] -> PathConsumer
mkPathConsumer = PathConsumer S.empty 

-- | get the next path element
pcGetNext :: PathConsumer -> Maybe T.Text
pcGetNext = headMay . _pcUnconsumed

-- | move forward in the path
pcConsumeNext :: PathConsumer -> PathConsumer
pcConsumeNext = execState $ do
    next <- uses pcUnconsumed headMay
    pcConsumed %= maybe id (flip snoc) next
    pcUnconsumed %= tailSafe
