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
module Web.ReqRes.Types where

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

import Control.Lens ((%~), makeLenses, snoc, (%=), uses, view)
import Safe (headMay, tailSafe)

class ToResponse a where
    toResponse :: a -> Response

instance ToResponse Response where
    toResponse = id

type Responder = Response -> IO ResponseReceived

data PathConsumer = PathConsumer {
    _pcConsumed :: S.Seq T.Text,
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

-- | this class is the api for building your handler
class (Functor m, MonadIO m) => MonadRespond m where
    -- | respond.
    respond :: ToResponse v => v -> m ResponseReceived
    -- | get out the request
    getRequest :: m Request
    -- | get the handlers
    getREHs :: m RequestErrorHandlers
    -- | get something out of the handlers
    getREH :: (RequestErrorHandlers -> a) -> m a
    getREH f = f <$> getREHs
    -- | use a function of the current request handlers for the inner
    -- action
    withREHs :: (RequestErrorHandlers -> RequestErrorHandlers) -> m a -> m a
    -- | get the path consumer as it stands here
    getPath :: m PathConsumer
    -- | use a function of the current path consumer
    withPath :: (PathConsumer -> PathConsumer) -> m a -> m a
    -- | use this path
    usePath :: PathConsumer -> m a -> m a
    usePath = withPath . const
    -- | get the current consumed path
    getConsumedPath :: m (S.Seq T.Text)
    getConsumedPath = _pcConsumed <$> getPath
    -- | get current unconsumed path
    getUnconsumedPath :: m [T.Text]
    getUnconsumedPath = _pcUnconsumed <$> getPath
    -- | get the next segment if there are any more segments
    getNextSegment :: m (Maybe T.Text)
    getNextSegment = headMay <$> getUnconsumedPath
    -- | consume the next segment
    withNextSegmentConsumed :: m a -> m a
    withNextSegmentConsumed = withPath pcConsumeNext

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
    _pathConsumer :: PathConsumer
}

makeLenses ''RespondData

-- | RespondT is a monad transformer that can implement MonadRespond
newtype RespondT m a = RespondT { 
    unRespondT :: ReaderT RespondData m a 
} deriving (Functor, Applicative, Monad, MonadReader RespondData)

instance (Functor m, MonadIO m) => MonadRespond (RespondT m) where
    respond v = view responder >>= \r -> liftIO . r . toResponse $ v
    getRequest = view request
    getREHs = view rehs
    withREHs handlers = local (rehs %~ handlers)
    getPath = view pathConsumer
    withPath f = local (pathConsumer %~ f) 

runRespondT :: RespondT m a -> RequestErrorHandlers -> Request -> Responder -> m a
runRespondT (RespondT act) handlers req res = runReaderT act $ RespondData handlers req res (mkPathConsumer $ pathInfo req)

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

-- | the PathMatcher makes it easy to provide actions for different paths.
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
    runPathExtractor :: StateT PathConsumer Maybe l
} deriving (Functor, Applicative, Monad, Alternative, MonadState PathConsumer, MonadPlus)

asPathExtractor :: Maybe a -> PathExtractor a
asPathExtractor = PathExtractor . lift

type HList0 = HList '[]

type HList1 a = HList '[a]

type PathExtractor0 = PathExtractor HList0

type PathExtractor1 a = PathExtractor (HList1 a)
