{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Api.Routes where

import Control.Applicative ((<$>), (<|>))
import Network.Wai
import Data.Monoid ((<>))
import Control.Monad ((>=>))
import Data.Aeson
import Network.HTTP.Types.Status
import Control.Lens ((<&>))
import Control.Monad.Trans.Cont (cont, evalCont)
import Data.Bool (bool)

import Web.Respond

import Chatless.Model.ID
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp

import Api.Config
import Api.Monad
import Api.Auth

import qualified Api.Queries as Q
import qualified Api.Ops as Ops

type CLApi = RespondT Chatless
type RR = ResponseReceived

apiApplication :: CLConfig -> Application
apiApplication conf = respondAppDefault (`runChatless` conf) api 

api :: CLApi RR
api = matchPath $
    pathEndOrSlash apiRoot <|> 
    path (seg "me") meRoutes <|>
    path localUserExtractor (getLocalUserRef >=> localUserRoutes) <|>
    path anyUserExtractor anyUserRoutes

apiRoot :: CLApi RR
apiRoot = matchMethod $ onGET $ do
    sid <- getServerId 
    respondOk (object ["server" .= sid])

meRoutes :: CLApi RR
meRoutes = authenticate callerAuth $ \callerData -> matchPath $
    path endOrSlash (respondOk $ Json callerData) <|>
    path (seg "sub") (handleSubs callerData) <|>
    path (seg "about") (callerTopic callerData Tp.userAboutTopicRef) <|>
    path (seg "invite") (callerTopic callerData Tp.userInviteTopicRef) <|>
    path (seg "topic" </> endOrSlash) (matchMethod $
         onGET (Q.getUserTopics callerData >>= respondOk . Json) <>
         onPOST (handleTopicCreate callerData)) <|>
    path topicIdSeg (callerTopic callerData . Tp.topicRefFromUser)

handleTopicCreate :: Ur.User -> CLApi RR
handleTopicCreate caller = withRequiredBody $ Ops.createTopic caller . getJsonS >=> Ops.respondTopicCreateResult

handleSubs :: Ur.User -> CLApi RR
handleSubs _ = rNotImplemented

localUserRoutes :: Ur.UserRef -> CLApi RR
localUserRoutes = Q.withUser $ \userData -> matchPath $
    pathEndOrSlash (respondOk $ Json userData) <|>
    path (seg "about") (otherUserTopic userData Tp.userAboutTopicRef) <|>
    path (seg "invite") (otherUserTopic userData Tp.userInviteTopicRef) <|>
    pathLastSeg "topic" (matchGET $ 
         Q.getUserTopics userData >>= respondOk . Json) <|>
    path topicIdSeg (otherUserTopic userData . Tp.topicRefFromUser)

anyUserRoutes :: Ur.UserRef -> CLApi RR
anyUserRoutes user = getUserRefIsLocal user >>= bool rNotImplemented (localUserRoutes user)

getUserRefIsLocal :: MonadChatless m => Ur.UserRef -> m Bool
getUserRefIsLocal uref = (Ur.userRefServer uref ==) <$> getServerId

callerTopic :: Ur.User -> (Ur.User -> Tp.TopicRef) -> CLApi RR
callerTopic user f = topicRoutes (Just user) (f user)

otherUserTopic :: Ur.User -> (Ur.User -> Tp.TopicRef) -> CLApi RR
otherUserTopic user f = topicRoutes Nothing (f user)

topicRoutes :: Maybe Ur.User -> Tp.TopicRef -> CLApi RR
topicRoutes = Q.withTopic . topicRoutesInner

topicRoutesInner :: Maybe Ur.User -> Tp.Topic -> CLApi RR
topicRoutesInner maybeCaller topicData = matchPath $
    pathEndOrSlash (matchGET $ Q.getTopicForCall maybeCaller topicData) <|>
    pathLastSeg "banner" (matchMethod $
        onGET (getField Tp.topicBanner) <>
        onPUT (performOp getTextBodyS Ops.changeBanner)) <|>
    pathLastSeg "info" (matchMethod $
        onGET (getField Tp.topicInfo) <>
        onPUT (performOp getJson Ops.changeInfo)) <|>
    pathLastSeg "mode" (matchMethod $ 
        onGET (getField Tp.topicMode) <>
        onPUT (performOp getJson Ops.changeTopicMode)) <|>
    path (seg "member") (matchPath $
        pathEndOrSlash (matchGET $ Q.listTopicMembersForCall maybeCaller topicData) <|>
        path (localUserExtractor </> endOrSlash) (getLocalUserRef >=> memberRoute) <|>
        path (anyUserExtractor </> endOrSlash) memberRoute) <|>
    path (seg "message") (matchPath $
        pathEndOrSlash (matchMethod $
            onGET (mLast $ Just $ Natural 1) <>
            onPOST (performOp getJson Ops.sendMessage)) <|>
        pathGET (seg "just"   </> midSeg </> endOrSlash) mJust <|>
        pathGET (seg "first"  </> optCountEndSeg) mFirst <|>
        pathGET (seg "last"   </> optCountEndSeg) mLast <|>
        pathGET (seg "before" </> midSeg </> optCountEndSeg) mBefore <|>
        pathGET (seg "after"  </> midSeg </> optCountEndSeg) mAfter <|>
        pathGET (seg "at"     </> midSeg </> optCountEndSeg) mAt <|>
        pathGET (seg "from"   </> midSeg </> optCountEndSeg) mFrom)
    where
    mJust   = Q.justMessage    maybeCaller topicData
    mFirst  = Q.messagesFirst  maybeCaller topicData
    mLast   = Q.messagesLast   maybeCaller topicData
    mBefore = Q.messagesBefore maybeCaller topicData
    mAfter  = Q.messagesAfter  maybeCaller topicData
    mAt     = Q.messagesAt     maybeCaller topicData
    mFrom   = Q.messagesFrom   maybeCaller topicData
    performOp :: (FromBody e b) => (b -> v) -> (Ur.User -> Tp.Topic -> v -> CLApi Ops.TopicOpResult) -> CLApi RR
    performOp = hPerformOperation maybeCaller topicData
    getField :: ToJSON a => (Tp.Topic -> a) -> CLApi RR
    getField = Q.getTopicFieldForCall maybeCaller topicData
    -- queries and ops for a particular member of the topic
    memberRoute :: Ur.UserRef -> CLApi RR
    memberRoute ur = matchMethod $
        onGET  (Q.getTopicMemberForCall maybeCaller topicData ur) <>
        onPUT  (performOp getJson $ Ops.changeMemberMode ur) <>
        onPOST (performOp getJson $ Ops.sendInvite ur)

hPerformOperation :: (FromBody e b) => Maybe Ur.User -> Tp.Topic -> (b -> v) -> (Ur.User -> Tp.Topic -> v -> CLApi Ops.TopicOpResult) -> CLApi RR
hPerformOperation maybeCaller topicData fGetBody c = evalCont $ do
    caller <- cont $ callerReauth maybeCaller
    body <- fGetBody <$> cont withRequiredBody
    return $ c caller topicData body >>= Ops.respondTopicOpResult

midSeg :: PathExtractor1 MessageId
midSeg = value

rNotImplemented :: MonadRespond m => m RR
rNotImplemented = respondEmptyBody notImplemented501 []

localUserExtractor :: PathExtractor1 (ServerId -> Ur.UserRef)
localUserExtractor = (seg "user" </> value) <&> hListMapTo1 (flip Ur.UserCoordKey)

getLocalUserRef :: MonadChatless m => (ServerId -> Ur.UserRef) -> m Ur.UserRef
getLocalUserRef = (<$> getServerId)

anyUserExtractor :: PathExtractor1 Ur.UserRef
anyUserExtractor = (seg "server" </> value </> seg "user" </> value) <&> hListMapTo1 Ur.UserCoordKey

topicIdSeg :: PathExtractor1 TopicId
topicIdSeg = seg "topic" </> value

optCountEndSeg :: PathExtractor1 (Maybe Natural) 
optCountEndSeg = ((value </> endOrSlash) <&> hListMapTo1 Just) <|> (endOrSlash <&> hListMapTo1 Nothing)

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
