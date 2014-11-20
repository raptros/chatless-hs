{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.ReqRes.Request where

import Control.Applicative
import Network.Wai
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.State as StateT
import Network.HTTP.Types.Method
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import Control.Lens (at, (^.), (<&>), to)
import Data.Maybe (fromMaybe)
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
matchPath pm = getPath >>= (fromMaybe handleUnmatchedPath . runPathMatcher pm)

path :: MonadRespond m => PathExtractor (HList l) -> HListElim l (m a) -> PathMatcher (m a)
path extractor f = PathMatcher $ \p -> do
    (v, p') <- pathExtract extractor p
    let action = hListUncurry f v
    Just $ usePath p' action

(</>) :: PathExtractor (HList l) -> PathExtractor (HList r) -> PathExtractor (HList (HAppendList l r))
(</>) = liftA2 hAppendList

pathEnd :: PathExtractor0
pathEnd = State.get >>= maybe (return HNil) (const empty) . pcGetNext

singleSegExtractor :: (T.Text -> Maybe (HList a)) -> PathExtractor (HList a)
singleSegExtractor extractor = do
    pz <- State.get
    res <- asPathExtractor . (pcGetNext >=> extractor) $ pz
    State.put (pcConsumeNext pz)
    return res

unitExtractor :: (T.Text -> Maybe ()) -> PathExtractor0
unitExtractor = singleSegExtractor . (fmap (const HNil) .)

predicateExtractor :: (T.Text -> Bool) -> PathExtractor0
predicateExtractor = unitExtractor . (mayWhen () .)

seg :: T.Text -> PathExtractor0
seg = predicateExtractor . (==)

pathExtract :: PathExtractor a -> PathConsumer -> Maybe (a, PathConsumer)
pathExtract extractor = StateT.runStateT (runPathExtractor extractor) 

singleItemExtractor :: (T.Text -> Maybe a) -> PathExtractor1 a
singleItemExtractor = singleSegExtractor . (fmap (hEnd . hBuild) .)

value :: PathPiece a => PathExtractor1 a
value = singleItemExtractor fromPathPiece

-- utility method
mayWhen :: a -> Bool -> Maybe a
mayWhen v True = Just v
mayWhen _ False = Nothing

