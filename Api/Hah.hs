{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Api.Hah where

import Data.Monoid ((<>))
import Control.Applicative ((<$>), Applicative, (<|>))
import Model.ID
import Model.User
import Model.Topic
import Network.Wai
import Control.Monad ((>=>))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString as BS
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map.Lazy as Map
import Control.Monad.Reader.Class (MonadReader)
import Control.Lens (at, (^.), (&), (?~), (%~), makeLenses, view, to, (<&>))
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Web.Respond
import Web.Respond.HListUtils
import Database.Groundhog
import Database.Groundhog.Generic

import Api.Config
import Api.Monad

type CLApi = RespondT Chatless

apiApplication :: CLConfig -> Application
apiApplication conf = respondAppDefault (`runChatless` conf) api 

api :: CLApi ResponseReceived
api = matchPath $
    path endOrSlash apiRoot <|> 
    path (seg "me") meRoutes <|>
    path localUserExtractor (getLocalUserRef >=> localUserRoutes) <|>
    path anyUserExtractor anyUserRoutes

apiRoot :: CLApi ResponseReceived
apiRoot = matchMethod $ onGET $ do
    sid <- getServerId 
    respond $ OkJson (object ["server" .= sid])

meRoutes :: CLApi ResponseReceived
meRoutes = authenticate callerAuthenticator $ \callerData -> do
    let handleTopic topicId = topicRoutes (fromUserRef topicId (extractUnique callerData))
    matchPath $
        path endOrSlash (respond $ OkJson callerData) <|>
        path (seg "about") (handleTopic (userAbout callerData)) <|>
        path (seg "invite") (handleTopic (userInvite callerData)) <|>
        path (seg "sub") (handleSubs callerData) <|>
        path topicIdSeg handleTopic

handleSubs :: UserRef -> CLApi ResponseReceived
handleSubs callerData = respond $ EmptyBody notImplemented501 []

-- | todo v important
callerAuthenticator :: CLApi (Maybe User)
callerAuthenticator = return Nothing

localUserRoutes :: UserRef -> CLApi ResponseReceived
localUserRoutes user = runQuery (getBy user) >>= maybe userNotFound userPaths
    where
    handleTopic = topicRoutes . flip fromUserRef user
    userNotFound = respond $ DefaultHeaders notFound404 $ ErrorReport "user_not_found" Nothing Nothing
    userPaths userData = matchPath $
        path endOrSlash (respond $ OkJson userData) <|>
        path (seg "about") (handleTopic (userAbout userData)) <|>
        path (seg "invite") (handleTopic (userInvite userData)) <|>
        path topicIdSeg handleTopic

anyUserRoutes :: UserRef -> CLApi ResponseReceived
anyUserRoutes user =  getServerId >>= \sid -> if (sid == userRefServer user) then localUserRoutes user else respond $ EmptyBody notImplemented501 []

topicRoutes :: TopicRef -> CLApi ResponseReceived
topicRoutes topic = respond $ DefaultHeaders notImplemented501 $ object ["topic" .= topic]

localUserExtractor :: PathExtractor1 (ServerId -> UserRef)
localUserExtractor = (seg "user" </> value) <&> hListMapTo1 (flip UserCoordKey)

getLocalUserRef :: MonadChatless m => (ServerId -> UserRef) -> m UserRef
getLocalUserRef = (<$> getServerId)

anyUserExtractor :: PathExtractor1 UserRef
anyUserExtractor = (seg "server" </> value </> seg "user" </> value) <&> hListMapTo1 UserCoordKey

topicIdSeg :: PathExtractor1 TopicId
topicIdSeg = seg "topic" </> value
