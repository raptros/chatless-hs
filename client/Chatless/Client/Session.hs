{-|
Description: chatless api session management

types and tools for talking to chatless
-}
module Chatless.Client.Session where

import qualified Data.Text.Encoding as Tenc
import qualified Data.ByteString as BS

import Control.Applicative ((<$>))
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Network.HTTP.Client as C

import Chatless.Model.ID (UserId(UserId))
import Chatless.Client.PathPointers

-- * session data types

-- | a pointer to the chatless api to connect to.
data ApiBase = ApiBase {
    -- | specify the host of the API.
    apiHost :: BS.ByteString,
    -- | specify the port of the API
    apiPort :: Int,
    -- | specify the path the API is hosted on (e.g. if the API is on the
    -- root of the server, this is the empty list).
    apiPath :: Path
} deriving (Eq, Show)

-- | specify how the session should authenticate each request to the API
data Authentication = 
    -- | add the "x-chatless-test-uid" header to each request. it is fake.
    FakeAuthentication UserId | 
    -- | do not perform any authentication
    NoAuthentication 
    deriving (Eq, Show)

-- | wrap up the 'Authentication', the 'ApiBase', and the 'Manager' into
-- one convenient bundle.
data Session = Session {
    sessionAuth :: Authentication,
    sessionApi :: ApiBase,
    sessionManager :: C.Manager
}

-- ** convenience constructors

-- | construct an ApiBase that points to the root of the server
apiBaseAtRoot :: BS.ByteString -> Int -> ApiBase
apiBaseAtRoot h p = ApiBase h p emptyPath

-- * session tools

-- | create a new session by construction a new manager (see
-- 'Network.HTTP.Client.newManager') that will modify each request using
-- 'authModRequest', and wrapping it up with the other passed information
-- into a Session record.
newSession :: (Functor m, MonadIO m) => ApiBase -> Authentication -> C.ManagerSettings -> m Session
newSession api auth baseSettings = Session auth api <$> liftIO (C.newManager $ baseSettings { C.managerModifyRequest = authModRequest auth })

-- | closes a 'Session' by performing 'Network.HTTP.Client.closeManager' on
-- it.
closeSession :: MonadIO m => Session -> m ()
closeSession ses = liftIO $ C.closeManager (sessionManager ses)

-- | 'Control.Monad.Catch.bracket's the passed continuation - it opens
-- a 'newSession' to run the continuation and then applies 'closeSession'
-- no matter how the inner action terminates.
withSession :: (MonadIO m, MonadMask m, Functor m) => ApiBase -> Authentication -> C.ManagerSettings -> (Session -> m a) -> m a
withSession api auth settings = bracket (newSession api auth settings) closeSession 

-- ** setting up authentication.

-- | modify a Request so that it performs the specified
-- Authentication.
authModRequest :: Authentication -> C.Request -> IO C.Request
authModRequest (FakeAuthentication (UserId uid)) r = return $ r { C.requestHeaders = ("x-chatless-test-uid", Tenc.encodeUtf8 uid) : C.requestHeaders r }
authModRequest NoAuthentication r = return r

