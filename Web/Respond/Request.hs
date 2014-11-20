{-|
Description: helpers for matching requests

contains various matching utilities
-}
module Web.Respond.Request where

import Network.Wai
import Network.HTTP.Types.Method
import qualified Data.Map.Lazy as Map
import Control.Lens (at, (^.), (<&>), to)
import Data.Maybe (fromMaybe)

import Data.Monoid
import Data.Function (on)

import Web.Respond.Monad
import Web.Respond.Response

-- * matching methods
--
-- | Map from method to thing. use it as a monoid.
--
-- > onGET action1 <> onPUT action2
newtype MethodMatcher a = MethodMatcher { 
    getMethodMatcher :: Map.Map StdMethod a 
}

instance Monoid (MethodMatcher a) where
    mempty = MethodMatcher mempty
    mappend = (MethodMatcher .) . on mappend getMethodMatcher 

-- ** using the method matcher

-- | look up the request method in the MethodMatcher map and run the
-- action; fall back on 'handleUnsupportedMethod' if the method cannot be
-- parsed or is not in the map.
matchMethod :: MonadRespond m => MethodMatcher (m ResponseReceived) -> m ResponseReceived
matchMethod dispatcher = getRequest <&> (parseMethod . requestMethod) >>= either (handleUnsupportedMethod supported) selectMethod
    where
    supported = Map.keys (getMethodMatcher dispatcher)
    selectMethod mth = fromMaybe (handleUnsupportedMethod supported (renderStdMethod mth)) $ dispatcher ^. to getMethodMatcher . at mth

-- ** method matchers

-- | map a StdMethod to a thing.
onMethod :: StdMethod -> a -> MethodMatcher a
onMethod = (MethodMatcher .) . Map.singleton

onGET :: a -> MethodMatcher a
onGET = onMethod GET

onPOST :: a -> MethodMatcher a
onPOST = onMethod POST

onHEAD :: a -> MethodMatcher a
onHEAD = onMethod HEAD

onPUT :: a -> MethodMatcher a
onPUT = onMethod PUT

onDELETE :: a -> MethodMatcher a
onDELETE = onMethod DELETE

onTRACE :: a -> MethodMatcher a
onTRACE = onMethod TRACE

onCONNECT :: a -> MethodMatcher a
onCONNECT = onMethod CONNECT

onOPTIONS :: a -> MethodMatcher a
onOPTIONS = onMethod OPTIONS

onPATCH :: a -> MethodMatcher a
onPATCH = onMethod PATCH

