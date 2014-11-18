{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.ReqRes.Types (
    ToResponse,
    toResponse,
    Responder,
    MonadRespond, 
    respond,
    getRequest,
    getREHs, 
    getREH,
    withREHs,
    getConsumedPath,
    getUnconsumedPath,
    getNextSegment,
    withNextSegmentConsumed,
    RequestErrorHandlers(..),
    rehUnknownMethod,
    rehUnsupportedMethod,
    rehUnmatchedPath,
    RespondT,
    runRespondT,
    PathMatcher(..),
    --pathMatcher,
    --runPathMatcher,
    ) where

import Control.Applicative
import Control.Monad (ap)
import Model.ID
import Network.Wai
import Data.Aeson
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map.Lazy as Map
import Control.Lens (at, (^.), (&), (?~))
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT, MonadBaseControl, StM, liftBaseWith, defaultLiftBaseWith, restoreM, defaultRestoreM, ComposeSt)
import Control.Monad.Trans.Class
import qualified Data.Sequence as S

import Control.Lens (at, (^.), (&), (?~), (%~), makeLenses, view, to, snoc, (^?), _head)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Safe (headMay, tailSafe)

class ToResponse a where
    toResponse :: a -> Response

instance ToResponse Response where
    toResponse = id

type Responder = Response -> IO ResponseReceived

class (Functor m, MonadIO m) => MonadRespond m where
    respond :: ToResponse v => v -> m ResponseReceived
    getRequest :: m Request
    getREHs :: m RequestErrorHandlers
    getREH :: (RequestErrorHandlers -> a) -> m a
    getREH f = f <$> getREHs
    withREHs :: (RequestErrorHandlers -> RequestErrorHandlers) -> m a -> m a
    getConsumedPath :: m (S.Seq T.Text)
    getUnconsumedPath :: m [T.Text]
    getNextSegment :: m (Maybe T.Text)
    getNextSegment = headMay <$> getUnconsumedPath
    withNextSegmentConsumed :: m a -> m a

data RequestErrorHandlers = RequestErrorHandlers {
    _rehUnsupportedMethod :: MonadRespond m => [StdMethod] -> StdMethod -> m ResponseReceived,
    _rehUnknownMethod :: MonadRespond m => [StdMethod] -> BS.ByteString -> m ResponseReceived,
    _rehUnmatchedPath :: MonadRespond m => m ResponseReceived
}

makeLenses ''RequestErrorHandlers

data RespondData = RespondData {
    _rehs :: RequestErrorHandlers,
    _request :: Request,
    _responder :: Responder,
    _consumedPath :: S.Seq T.Text,
    _unconsumedPath :: [T.Text]
}

makeLenses ''RespondData

-- | RespondT is a monad transformer that can implement MonadRespond
newtype RespondT m a = RespondT { unRespondT :: ReaderT RespondData m a } deriving (Functor, Applicative, Monad, MonadReader RespondData)

instance (Functor m, MonadIO m) => MonadRespond (RespondT m) where
    respond v = view responder >>= \r -> liftIO . r . toResponse $ v
    getRequest = view request
    getREHs = view rehs
    withREHs handlers = local (rehs %~ handlers)
    getConsumedPath = view consumedPath
    getUnconsumedPath = view unconsumedPath
    withNextSegmentConsumed = local consumeNext


consumeNext :: RespondData -> RespondData
consumeNext prev = prev & 
    consumedPath %~ maybe id (flip snoc) (prev ^? unconsumedPath . _head) &
    unconsumedPath %~ tailSafe

runRespondT :: RespondT m a -> RequestErrorHandlers -> Request -> Responder -> m a
runRespondT (RespondT act) handlers req res = runReaderT act $ RespondData handlers req res S.empty (pathInfo req)

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
    runPathMatcher :: [T.Text] -> Maybe a
}

instance Functor PathMatcher where
    fmap f pm = PathMatcher $ fmap f . runPathMatcher pm

instance Applicative PathMatcher where
    pure v = PathMatcher $ pure $ pure v
    f <*> r = PathMatcher $ \l -> runPathMatcher f l <*> runPathMatcher r l

instance Alternative PathMatcher where
    empty = PathMatcher $ const Nothing
    (<|>) l r = PathMatcher $ \segs -> runPathMatcher l segs <|> runPathMatcher r segs

instance Monad PathMatcher where
    return = pure
    a >>= f = PathMatcher $ \l -> runPathMatcher a l >>= \v -> runPathMatcher (f v) l
