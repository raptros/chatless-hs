{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.ReqRes.Request where

import Control.Applicative
import Network.Wai
import Network.HTTP.Types.Method
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import Control.Lens (at, (^.), (<&>), to)
import Data.Maybe (fromMaybe)
import Control.Monad ((>=>))
import Safe (headMay)
import Web.PathPieces
import Data.HList

import Web.ReqRes.Types
import Web.ReqRes.Response

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

matchMethod :: MonadRespond m => MethodMatcher (m ResponseReceived) -> m ResponseReceived
matchMethod dispatcher = getRequest <&> (parseMethod . requestMethod) >>= either (handleUnsupportedMethod supported) selectMethod
    where
    supported = Map.keys (getMethodMatcher dispatcher)
    selectMethod mth = fromMaybe (handleUnsupportedMethod supported (renderStdMethod mth)) $ dispatcher ^. to getMethodMatcher . at mth

matchPath :: MonadRespond m => PathMatcher (m ResponseReceived) -> m ResponseReceived
matchPath pm = getUnconsumedPath >>= (fromMaybe handleUnmatchedPath . runPathMatcher pm)

rootMatcher :: a -> PathMatcher a
rootMatcher a = PathMatcher $ mayWhen a . null

segMatcher :: MonadRespond m => T.Text -> m a -> PathMatcher (m a)
segMatcher segWant act = PathMatcher $ headMay >=> matchSeg
    where matchSeg = mayWhen (withNextSegmentConsumed act) . (segWant ==)

nextMatcher :: MonadRespond m => (T.Text -> m a) -> PathMatcher (m a)
nextMatcher consumer = PathMatcher $ fmap (withNextSegmentConsumed . consumer) . headMay

nextMayMatcher :: MonadRespond m => (T.Text -> Maybe (m a)) -> PathMatcher (m a)
nextMayMatcher consumer = PathMatcher $ headMay >=> consumer

segParserMatcher :: (MonadRespond m, ToResponse e) => (T.Text -> Either e a) -> (a -> m ResponseReceived) -> PathMatcher (m ResponseReceived)
segParserMatcher parser action = nextMatcher $ either respond action . parser

mustParseSegMatcher :: MonadRespond m => (T.Text -> Maybe a) -> (a -> m ResponseReceived) -> PathMatcher (m ResponseReceived)
mustParseSegMatcher parser action = nextMatcher $ \seg -> maybe (handlePathParseFailed [seg]) action (parser seg)

mayParseSegMatcher :: MonadRespond m => (T.Text -> Maybe a) -> (a -> m ResponseReceived) -> PathMatcher (m ResponseReceived)
mayParseSegMatcher parser action = nextMayMatcher $ fmap action . parser

mustBePathPiece :: (MonadRespond m, PathPiece s) => (s -> m ResponseReceived) -> PathMatcher (m ResponseReceived)
mustBePathPiece = mustParseSegMatcher fromPathPiece

mayBePathPiece :: (MonadRespond m, PathPiece s) => (s -> m ResponseReceived) -> PathMatcher (m ResponseReceived)
mayBePathPiece = mayParseSegMatcher fromPathPiece

-- utility method
mayWhen :: a -> Bool -> Maybe a
mayWhen v True = Just v
mayWhen _ False = Nothing


--i am not sure yet
{-
newtype MatchSegs l = SegMatcher {
    getMatchSegs :: T.Text -> Maybe l
}

data SegMatcher l 


(</>) :: MatchSegs l -> MatchSegs r 

-}
