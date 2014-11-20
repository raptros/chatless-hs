{-|
Description: path matching utility

This module provides the tools you need to match the path of a request, extract data
from it, and connect matches to Respond actions.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Respond.Path where

import Control.Applicative
import Network.Wai
import qualified Data.Text as T
import qualified Data.Sequence as S
import Safe (headMay)
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.State as StateT
import Control.Monad.Trans.Class
import Data.HList
import Web.PathPieces

import Web.Respond.Types
import Web.Respond.Monad
import Web.Respond.Response
import Web.Respond.HListUtils

-- * matching paths to actions

-- | the PathMatcher makes it easy to provide actions for different paths.
-- you use 'matchPath' to run it.
--
-- you can use this as a monad, but tbh you probably just want to use the
-- 'Applicative' and especially 'Alternative' instances.
newtype PathMatcher a = PathMatcher {
    runPathMatcher :: PathConsumer -> Maybe a
} 

instance Functor PathMatcher where
    fmap f pm = PathMatcher $ fmap f . runPathMatcher pm

instance Applicative PathMatcher where
    pure v = PathMatcher $ pure $ pure v
    f <*> r = PathMatcher $ (<*>) <$> runPathMatcher f <*> runPathMatcher r

instance Alternative PathMatcher where
    empty = PathMatcher $ const Nothing
    l <|> r = PathMatcher $ (<|>) <$> runPathMatcher l <*> runPathMatcher r

instance Monad PathMatcher where
    return = pure
    a >>= f = PathMatcher $ (>>=) <$> runPathMatcher a  <*> flip (runPathMatcher . f)

-- | run a path matcher containing a respond action against the current
-- path. uses the currently installed unmatched path handler if the match
-- fails.
--
-- see 'handleUnmatchedPath'
matchPath :: MonadRespond m => PathMatcher (m ResponseReceived) -> m ResponseReceived
matchPath pm = getPath >>= (fromMaybe handleUnmatchedPath . runPathMatcher pm)

-- * extracting path elements

-- | the path extractor matches the path and extracts values; it is useful
-- for building PathMatchers. it is built on both State and Maybe - if it
-- succeeds, it can modify the state to represent the path it has consumed.
newtype PathExtractor l = PathExtractor {
    runPathExtractor :: StateT.StateT PathConsumer Maybe l
} deriving (Functor, Applicative, Monad, Alternative, State.MonadState PathConsumer, MonadPlus)

-- | takes a Maybe and makes it into a path extractor
asPathExtractor :: Maybe a -> PathExtractor a
asPathExtractor = PathExtractor . lift

-- | a path extractor that extracts nothing, just matches
type PathExtractor0 = PathExtractor HList0

-- | a path extractor that extracts a single value from the path
type PathExtractor1 a = PathExtractor (HList1 a)

-- ** using path extractors

-- | runs a 'PathExtractor' against a 'PathConsumer'.
pathExtract :: PathExtractor a -> PathConsumer -> Maybe (a, PathConsumer)
pathExtract extractor = StateT.runStateT (runPathExtractor extractor) 

-- | create a 'PathMatcher' by providing a path extractor and an action that
-- consumes the extracted elements.
--
-- note that 'HListElim' is just a function from the types extracted to
-- something else
--
-- > path ((value :: PathExtractor1 String) </> seg "whatever" </> (value :: PathExtractor1 Integer)) $ \string integer -> -- some action
path :: MonadRespond m => PathExtractor (HList l) -> HListElim l (m a) -> PathMatcher (m a)
path extractor f = PathMatcher $ \p -> do
    (v, p') <- pathExtract extractor p
    let action = hListUncurry f v
    return $ usePath p' action

-- | combine two path extractors in sequence.
(</>) :: PathExtractor (HList l) -> PathExtractor (HList r) -> PathExtractor (HList (HAppendList l r))
(</>) = liftA2 hAppendList

-- ** useful path extractors

-- | match only when the PathConsumer in the path state has no unconsumed
-- elements.
pathEnd :: PathExtractor0
pathEnd = State.get >>= maybe (return HNil) (const empty) . pcGetNext

-- | build a path matcher that runs an extractor function on a single
-- element and then advances the path state if it matched. 
singleSegExtractor :: (T.Text -> Maybe (HList a)) -> PathExtractor (HList a)
singleSegExtractor extractor = do
    res <- State.get >>= asPathExtractor . (pcGetNext >=> extractor)
    State.modify pcConsumeNext 
    return res

-- | build an extractor from a function that does not produce any real
-- value
unitExtractor :: (T.Text -> Maybe ()) -> PathExtractor0
unitExtractor = singleSegExtractor . (fmap (const HNil) .)

-- | convert a predicate into a 'PathExtractor0'
predicateExtractor :: (T.Text -> Bool) -> PathExtractor0
predicateExtractor = unitExtractor . (mayWhen () .)

-- | WAI represents a trailing slash by having a null text as the last
-- element in the list. this matches it. it's just
--
-- @
-- 'predicateExtractor' 'Data.Text.null'
-- @
slashEnd :: PathExtractor0
slashEnd = predicateExtractor T.null

-- | best way to match the path end. it's just
--
-- @ 
-- 'pathEnd' 'Control.Applicative.<|>' 'slashEnd'
-- @
endOrSlash :: PathExtractor0
endOrSlash = pathEnd <|> slashEnd

-- | require that a segment be a certain string.
seg :: T.Text -> PathExtractor0
seg = predicateExtractor . (==)

-- | an extractor that takes a single path element and produces a single
-- value
singleItemExtractor :: (T.Text -> Maybe a) -> PathExtractor1 a
singleItemExtractor = singleSegExtractor . (fmap (hEnd . hBuild) .)

-- | if you have a 'PathPiece' instance for some type, you can extract it
-- from the path.
value :: PathPiece a => PathExtractor1 a
value = singleItemExtractor fromPathPiece

-- * utilities

-- | utility method for conditionally providing a value
mayWhen :: a -> Bool -> Maybe a
mayWhen v True = Just v
mayWhen _ False = Nothing

-- | run the inner action with a set path state.
--
-- > usePath = withPath . const
usePath :: MonadRespond m => PathConsumer -> m a -> m a
usePath = withPath . const

-- | get the part of the path that's been consumed so far.
--
-- > getConsumedPath = _pcConsumed <$> getPath
getConsumedPath :: MonadRespond m => m (S.Seq T.Text)
getConsumedPath = _pcConsumed <$> getPath

-- | get the part of the path that has yet to be consumed.
--
-- > getUnconsumedPath = _pcUnconsumed <$> getPath
getUnconsumedPath :: MonadRespond m => m [T.Text]
getUnconsumedPath = _pcUnconsumed <$> getPath

-- | get the next unconsumed path segment if there is one
--
-- > headMay <$> getUnconsumedPath
getNextSegment :: MonadRespond m => m (Maybe T.Text)
getNextSegment = headMay <$> getUnconsumedPath

-- | run the inner action with the next path segment consumed.
--
-- > withNextSegmentConsumed = withPath pcConsumeNext
withNextSegmentConsumed :: MonadRespond m => m a -> m a
withNextSegmentConsumed = withPath pcConsumeNext

