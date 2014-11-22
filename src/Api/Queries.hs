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
import Web.Respond.HListUtils ()
import Control.Monad.Logger (MonadLogger)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Lens ((<&>))

import Api.Config
import Api.Monad

type MonadControlIO m = (MonadBaseControl IO m, MonadIO m, MonadLogger m)

-- | reportable errors for query failures.
data QueryFailure =
    UserNotFound Ur.UserRef |
    TopicNotFound Tp.TopicRef

instance ReportableError QueryFailure where
    toErrorReport (UserNotFound ur) = ErrorReport "not_found" Nothing (Just $ object ["user" .= ur])
    toErrorReport (TopicNotFound tr) = ErrorReport "not_found" Nothing (Just $ object ["topic" .= tr])

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
