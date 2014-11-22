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
    pathEndOrSlash (matchGET getTopicR) <|>
    pathLastSeg "banner" (matchMethod $
        onGET (allowGetField topicBanner) <>
        onPUT (reauth $ const rNotImplemented)) <|>
    pathLastSeg "info" (matchMethod $
        onGET (allowGetField topicInfo) <>
        onPUT rNotImplemented) <|>
    pathLastSeg "mode" (matchMethod $ 
        onGET (allowGetField topicMode) <>
        onPUT rNotImplemented) <|>
    path (seg "member") (topicMemberRoutes maybeCaller topicData) <|> -- n.b. screwing this up breaks everything.
    path (seg "message") (matchMethod $
         onGET rNotImplemented <>
         onPOST (matchPath $ pathEndOrSlash $ rNotImplemented))
    where
    reauth :: (User -> CLApi ResponseReceived) -> CLApi ResponseReceived
    reauth = reauthenticate maybeCaller callerAuth
    authRequired = authenticatedOnly . topicMode $ topicData
    membershipRequired = membersOnly . topicMode $ topicData
    respondCensored = respond $ OkJson $ CensoredTopic topicData
    respondFull = respond $ OkJson topicData
    getTopicR
        | membershipRequired = tryGetAuth maybeCaller >>= maybe respondCensored (findMemberUser topicData >=> maybe respondCensored (const respondFull)) 
        | authRequired = maybe respondCensored (const respondFull) maybeCaller
        | otherwise = respondFull
    allowGetField f
        | membershipRequired = reauth (\caller -> withMemberAuth topicData caller (ErrorReport "not_member" Nothing Nothing) (const $ respond $ OkJson $ f topicData))
        | authRequired = reauth $ const $ respond $ OkJson $ f topicData
        | otherwise = respond $ OkJson $ f topicData


--mayUnless :: a -> Maybe b -> Maybe a
--mayUnless a = maybe (Just a) (const Nothing)

{-
authorizeTopicAction :: Topic -> (TopicMode -> MemberMode -> Bool) -> CLApi ResponseReceived -> User -> CLApi ResponseReceived
authorizeTopicAction tp p act caller = authorize authAction act
    where
    authAction = maybe False runPred <$> findMemberUser caller tp
    runPred = p (topicMode tp) . memberMode
    
reqReadConf :: Maybe User -> Topic -> CLApi ResponseReceived -> CLApi ResponseReceived
reqReadConf maybeCaller topicData modeCheck inner = go
    where
    reauth = reauthenticate maybeCaller callerAuth
    authRequired = authenticatedOnly . topicMode $ topicData
    membershipRequired = membersOnly . topicMode $ topicData
    go
        | membershipRequired = reauth (\caller -> authorize 
-}

mayUnless :: a -> Bool -> Maybe a
mayUnless _ True = Nothing
mayUnless a False = Just a

topicQueryGuard :: Maybe User -> Topic -> CLApi ResponseReceived -> CLApi ResponseReceived
topicQueryGuard maybeCaller topicData inner = go
    where
    reauth = reauthenticate maybeCaller callerAuth
    authRequired = authenticatedOnly . topicMode $ topicData
    membershipRequired = membersOnly . topicMode $ topicData
    go
        | membershipRequired = reauth (\caller -> 
                                        withMemberAuth topicData caller (ErrorReport "not_member" Nothing Nothing) $ \memberMode ->
                                            authorize (return $ mayUnless (ErrorReport "read_denied" Nothing Nothing) (mmRead memberMode)) inner)
        | authRequired = reauth (const inner)
        | otherwise = inner

-- | assume we've already matched the member segment
topicMemberRoutes :: Maybe User -> Topic -> CLApi ResponseReceived
topicMemberRoutes maybeCaller topicData = matchPath $
    pathEndOrSlash (matchGET $ tqd $ listTopicMembers topicData >>= respond . OkJson) <|>
    path (localUserExtractor </> endOrSlash) (getLocalUserRef >=> memberRoute) <|>
    path (anyUserExtractor </> endOrSlash) memberRoute
    where
    tqd = topicQueryGuard maybeCaller topicData
    memberRoute :: UserRef -> CLApi ResponseReceived
    memberRoute ur = matchMethod $
        onGET rNotImplemented <>
        onPUT rNotImplemented <>
        onPOST rNotImplemented

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
