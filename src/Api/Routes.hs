{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Api.Routes where

import Control.Applicative ((<$>), (<|>))
import Model.ID
import Model.User
import Model.Topic
import Model.TopicMember
import Network.Wai
import Data.Monoid ((<>))
import Control.Monad ((>=>))
import Data.Aeson
import Network.HTTP.Types.Status
import Control.Lens ((<&>))
import Web.Respond
import Web.Respond.HListUtils
import Api.Config
import Api.Monad
import Api.Queries
import Api.Auth

type CLApi = RespondT Chatless

apiApplication :: CLConfig -> Application
apiApplication conf = respondAppDefault (`runChatless` conf) api 

api :: CLApi ResponseReceived
api = matchPath $
    pathEndOrSlash apiRoot <|> 
    path (seg "me") meRoutes <|>
    path localUserExtractor (getLocalUserRef >=> localUserRoutes) <|>
    path anyUserExtractor anyUserRoutes

apiRoot :: CLApi ResponseReceived
apiRoot = matchMethod $ onGET $ do
    sid <- getServerId 
    respond $ OkJson (object ["server" .= sid])

meRoutes :: CLApi ResponseReceived
meRoutes = authenticate callerAuth $ \callerData -> do
    let handleTopic = topicRoutes (Just callerData)
        meTopicsList =  matchMethod $
            onGET (getUserTopics callerData >>= respond . OkJson) <>
            onPOST (respond $ EmptyBody notImplemented501 [])
    matchPath $
        path endOrSlash (respond $ OkJson callerData) <|>
        path (seg "sub") (handleSubs callerData) <|>
        path (seg "about") (handleTopic (userAboutTopicRef callerData)) <|>
        path (seg "invite") (handleTopic (userInviteTopicRef callerData)) <|>
        path (seg "topic" </> endOrSlash) meTopicsList <|>
        path topicIdSeg (handleTopic . userTopicRef callerData)

handleSubs :: User -> CLApi ResponseReceived
handleSubs _ = respond $ EmptyBody notImplemented501 []

localUserRoutes :: UserRef -> CLApi ResponseReceived
localUserRoutes = withUser $ \userData -> do
    let userTopicsList = matchMethod $ onGET $ getUserTopics userData >>= respond . OkJson
        handleTopic = topicRoutes Nothing
    matchPath $
        path endOrSlash (respond $ OkJson userData) <|>
        path (seg "about") (handleTopic (userAboutTopicRef userData)) <|>
        path (seg "invite") (handleTopic (userInviteTopicRef userData)) <|>
        path (seg "topic" </> endOrSlash) userTopicsList <|>
        path topicIdSeg (handleTopic . userTopicRef userData)

anyUserRoutes :: UserRef -> CLApi ResponseReceived
anyUserRoutes user =  getServerId >>= \sid -> if sid == userRefServer user then localUserRoutes user else respond $ EmptyBody notImplemented501 []

topicRoutes :: Maybe User -> TopicRef -> CLApi ResponseReceived
topicRoutes = withTopic . topicRoutesInner

topicRoutesInner :: Maybe User -> Topic -> CLApi ResponseReceived
topicRoutesInner maybeCaller topicData = matchPath $
    pathEndOrSlash (matchGET $ getTopicForCall maybeCaller topicData) <|>
    pathLastSeg "banner" (matchMethod $
        onGET (getField topicBanner) <>
        onPUT rNotImplemented) <|>
    pathLastSeg "info" (matchMethod $
        onGET (getField topicInfo) <>
        onPUT rNotImplemented) <|>
    pathLastSeg "mode" (matchMethod $ 
        onGET (getField topicMode) <>
        onPUT rNotImplemented) <|>
    path (seg "member") (matchPath $
        pathEndOrSlash (matchGET $ listTopicMembersForCall maybeCaller topicData) <|>
        path (localUserExtractor </> endOrSlash) (getLocalUserRef >=> memberRoute) <|>
        path (anyUserExtractor </> endOrSlash) memberRoute) <|>
    path (seg "message") (topicMessageRoutes maybeCaller topicData)
    where
    getField :: ToJSON a => (Topic -> a) -> CLApi ResponseReceived
    getField = getTopicFieldForCall maybeCaller topicData
    -- queries and ops for a particular member of the topic
    memberRoute :: UserRef -> CLApi ResponseReceived
    memberRoute ur = matchMethod $
        onGET (getTopicMemberForCall maybeCaller topicData ur) <>
        onPUT rNotImplemented <>
        onPOST rNotImplemented

-- | assume we've matched the message segment
topicMessageRoutes :: Maybe User -> Topic -> CLApi ResponseReceived
topicMessageRoutes maybeCaller topicData = matchPath $
    pathEndOrSlash (matchMethod $
        onGET rNotImplemented <>
        onPOST rNotImplemented)
    where
    tqg :: CLApi ResponseReceived -> CLApi ResponseReceived
    tqg = topicQueryGuard maybeCaller topicData

rNotImplemented :: MonadRespond m => m ResponseReceived
rNotImplemented = respond $ EmptyBody notImplemented501 []

localUserExtractor :: PathExtractor1 (ServerId -> UserRef)
localUserExtractor = (seg "user" </> value) <&> hListMapTo1 (flip UserCoordKey)

getLocalUserRef :: MonadChatless m => (ServerId -> UserRef) -> m UserRef
getLocalUserRef = (<$> getServerId)

anyUserExtractor :: PathExtractor1 UserRef
anyUserExtractor = (seg "server" </> value </> seg "user" </> value) <&> hListMapTo1 UserCoordKey

topicIdSeg :: PathExtractor1 TopicId
topicIdSeg = seg "topic" </> value


{- todo
main api:
/me MeR GET
/me/about MeAboutTopicSubR TopicSub getMeAboutTopicSub 
/me/invite MeInviteTopicSubR TopicSub getMeInviteTopicSub
/me/topic MeTopicsR GET POST
/me/topic/#TopicId MeTopicSubR TopicSub getMeTopicSub
-- subscription system
/me/sub/user/#UserId/topic/#TopicId SubsLocalR PUT
-- local users
/user/#UserId LocalUserR GET
/user/#UserId/about LocalUserAboutTopicSubR TopicSub getLocalUserAboutTopicSub
/user/#UserId/invite LocalUserInviteTopicSubR TopicSub getLocalUserInviteTopicSub
/user/#UserId/topic LocalUserTopicsR GET
/user/#UserId/topic/#TopicId LocalUserTopicSubR TopicSub getLocalUserTopicSub
-- todo remote topics
-- /server/#ServerId/user/#UserId AnyUserR GET
-- /server/#ServerId/user/#UserId/topic AnyUserTopicsR GET
-- /server/#ServerId/user/#UserId/ AnyUserR GET
-- /server/#ServerId/user/#UserId/about AnyUserAboutTopicSubR TopicSub getAnyUserAboutTopicSub
-- /server/#ServerId/user/#UserId/invite AnyUserInviteTopicSubR TopicSub getAnyUserInviteTopicSub
-- /server/#ServerId/user/#UserId/topic/#TopicId AnyUserTopicSubR TopicSub getAnyUserTopicSub

topics api:
/ TopicR GET
/banner TopicBannerR GET PUT
/info TopicInfoR GET PUT
/mode TopicModeR GET PUT
/member MembersR GET
/member/user/#UserId LocalMemberR GET PUT POST
/member/server/#ServerId/user/#UserId MemberR GET PUT POST
/message MsgR GET POST
/message/first MsgFirst1R GET
-- todo limit the number of messages
/message/first/#Int MsgFirstR GET
/message/last MsgLast1R GET
/message/last/#Int MsgLastR GET
/message/before/#MessageId MsgBefore1R GET
/message/before/#MessageId/#Int MsgBeforeR GET
/message/after/#MessageId MsgAfter1R GET
/message/after/#MessageId/#Int MsgAfterR GET
/message/at/#MessageId MsgAt1R GET
/message/at/#MessageId/#Int MsgAtR GET
/message/from/#MessageId MsgFrom1R GET
/message/from/#MessageId/#Int MsgFromR GET
-}
