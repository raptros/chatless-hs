module ServerSetup (runWaiApp) where

import Control.Applicative ((<$>))
import Network.Wai
import Network.HTTP.Types.Header
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.Autohead as Autohead
import qualified Network.Wai.Middleware.AcceptOverride as AcceptOverride
import qualified Network.Wai.Middleware.MethodOverride as MethodOverride
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Middleware.Rewrite as Rewrite
import qualified Data.Default.Class as Def
import qualified Data.Text as T
import System.Log.FastLogger

runWaiApp :: Warp.Port -> LoggerSet -> Application -> IO ()
runWaiApp port logger app = (prepApp logger app) >>= Warp.run port

prepApp :: LoggerSet -> Application -> IO Application
prepApp logger app = ($ app) <$> mkMiddleware logger

mkMiddleware :: LoggerSet -> IO Middleware
mkMiddleware logger = (\rlmw -> rlmw . middlewares) <$> logMiddleware
    where
    middlewares = {- Autohead.autohead . -} Gzip.gzip Def.def . Rewrite.rewritePure (const . pathCleaner)
    logMiddleware = RequestLogger.mkRequestLogger $ Def.def {
        RequestLogger.outputFormat = RequestLogger.Apache RequestLogger.FromSocket,
        RequestLogger.destination = RequestLogger.Logger logger
    }
    
pathCleaner = filterLast (not . T.null) . fmap T.toLower

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast p (a:[]) = if (p a) then [a] else []
filterLast p (a:as) = a:(filterLast p as)

