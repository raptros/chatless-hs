{-# LANGUAGE FlexibleContexts #-}
module Api.Ops.Base where

import Data.Bool (bool)
import Data.Either (isRight)
import Control.Monad.Base (MonadBase)
import Control.Exception.Lifted (throwIO, Exception)
import Control.Applicative
import qualified Data.Foldable as Fld

-- * utils

-- | old, new, produce the new only if not the old
passNewIfChanged :: Eq a => a -> a -> Maybe a
passNewIfChanged old new = bool Nothing (Just new) (old /= new)
                   
-- | throws an exception when the Maybe is Nothing
throwOrReturn :: (MonadBase IO m, Exception e) => e -> Maybe a -> m a
throwOrReturn = flip maybe return . throwIO

-- | retrys the action up to n times.
retryNTimes :: (Alternative f) => Int -> f a -> f a
retryNTimes n act = Fld.asum $ replicate n act


whenRight :: a -> Either b c -> Maybe a
whenRight v = bool Nothing (Just v) . isRight

