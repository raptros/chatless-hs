{-|
Description: basic chatless API requests

these are the basic chatless API requests
-}

module Chatless.Client.Request where

import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Default as Def
import Control.Monad.Except

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import qualified Network.HTTP.Client as C

import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
import qualified Chatless.Model.Message as Msg

import Chatless.Client.Session
import Chatless.Client.Response
import Chatless.Client.Monad
import Chatless.Client.PathPointers

-- * the basic request
performRequest :: MChatlessClient m => (r -> Either ResponseError a) -> (Session -> m r) -> m a
performRequest process call = getSession >>= call >>= (either throwError return . process)

callApi :: (MChatlessClient m, ResponseInterpreter a) => Path -> StdMethod -> RequestHeaders -> C.RequestBody -> m a
callApi path method headers body = performRequest interpretResponse $ \session -> liftIO (C.httpLbs (mkRequest path method headers body session) (sessionManager session))

-- ** request construction

-- | constructs a call to the chatless API by combining the request spec
-- with session information and proper defaults.
mkRequest :: Path -> StdMethod -> RequestHeaders ->  C.RequestBody -> Session -> C.Request
mkRequest path method headers body session = Def.def {
    C.method = renderStdMethod method,
    C.host = apiHost . sessionApi $ session,
    C.port = apiPort . sessionApi $ session,
    C.path = mkAbsPath $ (apiPath . sessionApi $ session) <> path,
    C.requestHeaders = headers,
    C.requestBody = body,
    C.checkStatus = const (const (const Nothing))
}

-- * specific requests

-- ** all GET requests

queryApi :: (MChatlessClient m, ResponseInterpreter a) => Path -> m a
queryApi path =  callApi path GET [] (C.RequestBodyBS "")

queryApiJson :: (MChatlessClient m, FromJSON a) => Path -> m a
queryApiJson = fmap okJsonBody . queryApi

queryPtr :: (MChatlessClient m, FromJSON a, PathPointer p) => p -> m a
queryPtr = queryApiJson . toPath

queryPtrSub :: (MChatlessClient m, FromJSON a, PathPointer p) => T.Text -> p -> m a
queryPtrSub sub = queryApiJson . pathSub sub

getUser :: MChatlessClient m => UserPtr -> m Ur.User
getUser = queryPtr

getMe :: MChatlessClient m => m Ur.User
getMe = getUser MePtr

getUserTopics :: MChatlessClient m => UserPtr -> m [Tp.TopicRef]
getUserTopics = queryPtrSub userTopicsSub

listMeTopics :: MChatlessClient m => m [Tp.TopicRef]
listMeTopics = getUserTopics MePtr

getTopic :: MChatlessClient m => TopicPtr -> m Tp.Topic
getTopic = queryPtr

getTopicMembers :: MChatlessClient m => TopicPtr -> m [Tm.MemberPartial]
getTopicMembers = queryPtrSub topicMembersSub

getTopicMember :: MChatlessClient m => MemberPtr -> m Tm.MemberMode
getTopicMember = queryPtr

getMessages :: MChatlessClient m => MessageQueryPtr -> m [Msg.Message]
getMessages = queryPtr

getJustMessage :: MChatlessClient m => JustMessagePtr -> m Msg.Message
getJustMessage = queryPtr

-- ** put and post requests
sendBodyPath :: (MChatlessClient m, ResponseInterpreter a) => StdMethod -> BS.ByteString -> Path -> BSL.ByteString -> m a
sendBodyPath method contentType path body = callApi path method [(hContentType, contentType)] (C.RequestBodyLBS body)

sendBodyPtr :: (MChatlessClient m, PathPointer p, ResponseInterpreter a) => StdMethod -> BS.ByteString -> p -> BSL.ByteString -> m a
sendBodyPtr method contentType = sendBodyPath method contentType . toPath

sendBodyPtrSub :: (MChatlessClient m, PathPointer p, ResponseInterpreter a) => StdMethod -> BS.ByteString -> Sub -> p -> BSL.ByteString -> m a
sendBodyPtrSub method contentType sub = sendBodyPtr method contentType . pathSub sub

sendJsonPath ::(MChatlessClient m, ToJSON b, ResponseInterpreter a) => StdMethod -> Path -> b -> m a
sendJsonPath method path = sendBodyPath method "application/json" path . encode

sendJsonPtr :: (MChatlessClient m , ToJSON b, ResponseInterpreter a, PathPointer p) => StdMethod -> p -> b -> m a
sendJsonPtr method = sendJsonPath method . toPath

sendJsonPtrSub :: (MChatlessClient m , ToJSON b, ResponseInterpreter a, PathPointer p) => StdMethod -> Sub -> p -> b -> m a
sendJsonPtrSub method sub = sendJsonPtr method . pathSub sub

createTopic :: MChatlessClient m => Tp.TopicCreate -> m TopicCreateResult
createTopic = sendJsonPtrSub POST "topic" MePtr 

setTopicBanner :: MChatlessClient m => TopicPtr -> TL.Text -> m TopicOpResult
setTopicBanner ptr = sendBodyPtrSub PUT "text/plain" "banner" ptr . TL.encodeUtf8 

sendMessage :: MChatlessClient m => TopicPtr -> Value -> m TopicOpResult
sendMessage = sendJsonPtrSub POST "message"

setMemberMode :: MChatlessClient m => MemberPtr -> Tm.MemberModeUpdate -> m TopicOpResult
setMemberMode = sendJsonPtr PUT

sendInvite :: MChatlessClient m => MemberPtr -> Value -> m TopicOpResult
sendInvite = sendJsonPtr POST
