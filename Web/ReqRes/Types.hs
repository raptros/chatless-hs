{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.ReqRes.Types (
    ToResponse,
    toResponse,
    PathZipper,
    mkPathZipper,
    pzGetNext,
    pzConsumeNext,
    pzUnconsumed,
    Responder,
    MonadRespond, 
    respond,
    getRequest,
    getREHs, 
    getREH,
    withREHs,
    getPathZipper,
    withPathZipper,
    withPathZipper',
    getConsumedPath,
    getUnconsumedPath,
    getNextSegment,
    withNextSegmentConsumed,
    RequestErrorHandlers(..),
    rehUnsupportedMethod,
    rehUnmatchedPath,
    rehPathParseFailed,
    RespondT,
    runRespondT,
    PathMatcher(..),
    MethodMatcher(..),
    HListElim,
    hListUncurry,
    PathExtractor(..),
    HList0,
    HList1,
    PathExtractor0,
    PathExtractor1,
    asPathExtractor,
    --pathMatcher,
    --runPathMatcher,
    ) where

import Control.Applicative
import Data.Monoid
import Data.Function (on)
import Network.Wai
import Network.HTTP.Types.Method
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT, MonadBaseControl, StM, liftBaseWith, defaultLiftBaseWith, restoreM, defaultRestoreM, ComposeSt)
import Control.Monad.Trans.Class
import Data.HList
import qualified Data.Sequence as S

import Control.Lens ((%~), makeLenses, view, snoc, (^?), _head, (&), (.~))
import Safe (headMay, tailSafe)

class ToResponse a where
    toResponse :: a -> Response

instance ToResponse Response where
    toResponse = id

type Responder = Response -> IO ResponseReceived

data PathZipper = PathZipper {
    _pzConsumed :: S.Seq T.Text,
    _pzUnconsumed :: [T.Text]
} deriving (Eq, Show)

makeLenses ''PathZipper

mkPathZipper :: [T.Text] -> PathZipper
mkPathZipper path = PathZipper S.empty path

pzGetNext :: PathZipper -> Maybe T.Text
pzGetNext pz = headMay (_pzUnconsumed pz) 

pzConsumeNext :: PathZipper -> PathZipper
pzConsumeNext prev =  prev &
    pzConsumed %~ maybe id (flip snoc) (prev ^? pzUnconsumed . _head) &
    pzUnconsumed %~ tailSafe

class (Functor m, MonadIO m) => MonadRespond m where
    respond :: ToResponse v => v -> m ResponseReceived
    getRequest :: m Request
    getREHs :: m RequestErrorHandlers
    getREH :: (RequestErrorHandlers -> a) -> m a
    getREH f = f <$> getREHs
    withREHs :: (RequestErrorHandlers -> RequestErrorHandlers) -> m a -> m a
    getPathZipper :: m PathZipper
    withPathZipper :: (PathZipper -> PathZipper) -> m a -> m a
    withPathZipper' :: PathZipper -> m a -> m a
    withPathZipper' = withPathZipper . const
    getConsumedPath :: m (S.Seq T.Text)
    getConsumedPath = (view pzConsumed) <$> getPathZipper
    getUnconsumedPath :: m [T.Text]
    getUnconsumedPath = (view pzUnconsumed) <$> getPathZipper
    getNextSegment :: m (Maybe T.Text)
    getNextSegment = headMay <$> getUnconsumedPath
    withNextSegmentConsumed :: m a -> m a
    withNextSegmentConsumed = withPathZipper pzConsumeNext

data RequestErrorHandlers = RequestErrorHandlers {
    _rehUnsupportedMethod :: MonadRespond m => [StdMethod] -> Method -> m ResponseReceived,
    _rehUnmatchedPath :: MonadRespond m => m ResponseReceived,
    _rehPathParseFailed :: MonadRespond m => [T.Text] -> m ResponseReceived
}

makeLenses ''RequestErrorHandlers

data RespondData = RespondData {
    _rehs :: RequestErrorHandlers,
    _request :: Request,
    _responder :: Responder,
    _pathZipper :: PathZipper
}

makeLenses ''RespondData

-- | RespondT is a monad transformer that can implement MonadRespond
newtype RespondT m a = RespondT { unRespondT :: ReaderT RespondData m a } deriving (Functor, Applicative, Monad, MonadReader RespondData)

instance (Functor m, MonadIO m) => MonadRespond (RespondT m) where
    respond v = view responder >>= \r -> liftIO . r . toResponse $ v
    getRequest = view request
    getREHs = view rehs
    withREHs handlers = local (rehs %~ handlers)
    getPathZipper = view pathZipper
    withPathZipper f = local (pathZipper %~ f) 

runRespondT :: RespondT m a -> RequestErrorHandlers -> Request -> Responder -> m a
runRespondT (RespondT act) handlers req res = runReaderT act $ RespondData handlers req res (mkPathZipper $ pathInfo req)

instance MonadTrans RespondT where
    lift act = RespondT $ lift act

instance MonadIO m => MonadIO (RespondT m) where
    liftIO act = RespondT $ liftIO act

--these next three son of a gun all need UndecidableInstances

instance MonadBase b m => MonadBase b (RespondT m) where
    liftBase = liftBaseDefault

-- and these two demand TypeFamilies

instance MonadTransControl RespondT where
    newtype StT RespondT a = StRespond { unStRespond :: StT (ReaderT RespondData) a }
    liftWith = defaultLiftWith RespondT unRespondT StRespond
    restoreT = defaultRestoreT RespondT unStRespond

instance MonadBaseControl b m => MonadBaseControl b (RespondT m) where
    newtype StM (RespondT m) a = StMT { unStMT :: ComposeSt RespondT m a}
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM   unStMT

newtype PathMatcher a = PathMatcher {
    runPathMatcher :: PathZipper  -> Maybe a
}

instance Functor PathMatcher where
    fmap f pm = PathMatcher $ fmap f . runPathMatcher pm

instance Applicative PathMatcher where
    pure v = PathMatcher $ pure $ pure v
    f <*> r = PathMatcher $ (<*>) <$> runPathMatcher f <*> runPathMatcher r

instance Alternative PathMatcher where
    empty = PathMatcher $ const Nothing
    (<|>) l r = PathMatcher $ (<|>) <$> runPathMatcher l <*> runPathMatcher r

instance Monad PathMatcher where
    return = pure
    a >>= f = PathMatcher $ \l -> runPathMatcher a l >>= \v -> runPathMatcher (f v) l

newtype MethodMatcher a = MethodMatcher { 
    getMethodMatcher :: Map.Map StdMethod a 
}

instance Monoid (MethodMatcher a) where
    mempty = MethodMatcher mempty
    mappend = (MethodMatcher .) . on mappend getMethodMatcher 

type family HListElim (ts :: [*]) (a :: *) :: *
type instance HListElim '[] a = a
type instance HListElim (t ': ts) a = t -> HListElim ts a

hListUncurry :: HListElim ts a -> HList ts -> a
hListUncurry f HNil = f
hListUncurry f (HCons x xs) = hListUncurry (f x) xs

newtype PathExtractor l = PathExtractor {
    runPathExtractor :: StateT PathZipper Maybe l
} deriving (Functor, Applicative, Monad, Alternative, MonadState PathZipper, MonadPlus)

asPathExtractor :: Maybe a -> PathExtractor a
asPathExtractor = PathExtractor . lift

type HList0 = HList '[]

type HList1 a = HList '[a]

type PathExtractor0 = PathExtractor HList0

type PathExtractor1 a = PathExtractor (HList1 a)
