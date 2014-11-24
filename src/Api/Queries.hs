{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Queries where

import Control.Applicative ((<$>))
import qualified Model.User as Ur
import qualified Model.Topic as Tp
import qualified Model.TopicMember as Tm
import Network.Wai
import Data.Aeson
import Network.HTTP.Types.Status
import Web.Respond
import qualified Database.Groundhog as Gh
import qualified Data.Text as T
import Web.Respond.HListUtils ()
import Control.Monad.Logger (MonadLogger)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import Control.Lens ((<&>))

import Api.Config
import Api.Monad
import Api.Auth

type MonadControlIO m = (MonadBaseControl IO m, MonadIO m, MonadLogger m)

data QueryDenyReason =
    NotMember |
    ReadDenied

queryDenyReasonText :: QueryDenyReason -> T.Text
queryDenyReasonText NotMember = "not_member"
queryDenyReasonText ReadDenied = "read_denied"

-- | reportable errors for query failures.
data QueryFailure =
    UserNotFound Ur.UserRef |
    TopicNotFound Tp.TopicRef |
    MemberNotFound Tp.TopicRef Ur.UserRef |
    QueryDenied QueryDenyReason 

textNotFound :: T.Text
textNotFound = "not_found"

instance ReportableError QueryFailure where
    toErrorReport (UserNotFound ur) = errorReportWithDetails textNotFound $ object ["user" .= ur]
    toErrorReport (TopicNotFound tr) = errorReportWithDetails textNotFound $ object ["topic" .= tr]
    toErrorReport (MemberNotFound tr ur) = errorReportWithDetails textNotFound $ object ["topic" .= tr, "user" .= ur]
    toErrorReport (QueryDenied qdr) = simpleErrorReport (queryDenyReasonText qdr) 

-- | run an action on a user ... if that user can be found in the db
withUser :: (MonadChatless m, MonadRespond m) => (Ur.User -> m ResponseReceived) -> Ur.UserRef -> m ResponseReceived
withUser act uref = runQuery (Gh.getBy uref) >>= maybe notFound act
    where notFound = respond $ ResponseError notFound404 (UserNotFound uref)

userTopicsQuery :: MonadControlIO m => Ur.User -> Gh.DbPersist CLDb m [Tp.TopicRef]
userTopicsQuery user = Gh.project Tp.TopicCoord $ (Tp.TopicServerField Gh.==. Ur.userServer user) Gh.&&. (Tp.TopicUserField Gh.==. Ur.userId user)

-- | list out the topics a user has created
getUserTopics :: MonadChatless m => Ur.User -> m [Tp.TopicRef]
getUserTopics = runQuery . userTopicsQuery

-- | run the action given the topic, if the topic can be found
withTopic :: (MonadChatless m, MonadRespond m) => (Tp.Topic -> m ResponseReceived) -> Tp.TopicRef -> m ResponseReceived
withTopic act tref = runQuery (Gh.getBy tref) >>= maybe notFound act
    where notFound = respond $ ResponseError notFound404 (TopicNotFound tref)

findMember :: MonadChatless m => Tp.TopicRef -> Ur.UserRef -> m (Maybe Tm.Member)
findMember tr ur = runQuery $ Gh.getBy (Tm.TargetMemberKey tr ur) 

findMemberRef :: (MonadChatless m) => Tp.Topic -> Ur.UserRef -> m (Maybe Tm.Member)
findMemberRef = findMember . Tp.getRefFromTopic

findMemberUser :: MonadChatless m => Tp.Topic -> Ur.User -> m (Maybe Tm.Member)
findMemberUser tp us = findMember (Tp.getRefFromTopic tp) (Ur.getRefFromUser us)

withMemberAuth :: (MonadChatless m, MonadRespond m, ReportableError e) => Tp.Topic -> Ur.User -> e -> (Tm.MemberMode -> m ResponseReceived) -> m ResponseReceived
withMemberAuth topic user err = authorizeE (findMemberUser topic user <&> maybe (Left err) (Right . Tm.memberMode))

topicMembersQuery :: MonadControlIO m => Tp.TopicRef -> Gh.DbPersist CLDb m [Tm.MemberPartial]
topicMembersQuery tr = (fmap (uncurry Tm.MemberPartial)) <$> Gh.project (Tm.MemberUserField, Tm.MemberModeField) (Tm.MemberTopicField Gh.==. tr)

listTopicMembers :: MonadChatless m => Tp.Topic -> m [Tm.MemberPartial]
listTopicMembers = runQuery . topicMembersQuery . Tp.getRefFromTopic

authRequired :: Tp.Topic -> Bool
authRequired = Tp.authenticatedOnly . Tp.topicMode

membershipRequired :: Tp.Topic -> Bool
membershipRequired = Tp.membersOnly . Tp.topicMode

getTopicForCall :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> m ResponseReceived
getTopicForCall maybeCaller topicData
    | membershipRequired topicData = runMaybeT (do
        caller <- MaybeT $ tryGetAuth maybeCaller 
        member <- MaybeT $ findMemberUser topicData caller
        lift respondFull) >>= maybe respondCensored return
    | authRequired topicData = maybe respondCensored (const respondFull) maybeCaller
    | otherwise = respondFull
    where
    respondCensored = respond $ OkJson $ Tp.CensoredTopic topicData
    respondFull = respond $ OkJson topicData

getTopicFieldForCall :: (MonadRespond m, MonadChatless m, ToJSON v) => Maybe Ur.User -> Tp.Topic -> (Tp.Topic -> v) -> m ResponseReceived
getTopicFieldForCall maybeCaller topicData f
    | membershipRequired topicData = reauth $ \caller -> withMemberAuth topicData caller (QueryDenied NotMember) (const respondField)
    | authRequired topicData = reauth (const respondField)
    | otherwise = respondField
    where
    respondField = respond $ OkJson $ f topicData
    reauth = callerReauth maybeCaller 

topicQueryGuard :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> m ResponseReceived -> m ResponseReceived
topicQueryGuard maybeCaller topicData inner 
    | membershipRequired topicData = reauth $ \caller -> 
        withMemberAuth topicData caller (QueryDenied NotMember) $ \memberMode ->
            if Tm.mmRead memberMode then inner else handleDenied $ QueryDenied ReadDenied
    | authRequired topicData = reauth (const inner)
    | otherwise = inner
    where
    reauth = callerReauth maybeCaller 

