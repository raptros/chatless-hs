{-|
Description: topic and topic member queries

topic and topic member queries
-}

module Api.Queries.Topic where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Except

import Network.Wai (ResponseReceived)

import Data.Aeson (ToJSON)

import qualified Database.Groundhog as Gh

import Web.Respond
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.TopicMember as Tm
import Api.Queries.Base
import Api.Monad
import Api.Auth
import Control.Applicative ((<$>))

-- * topic queries

-- | run the action given the topic, if the topic can be found
withTopic :: (MonadChatless m, MonadRespond m) => (Tp.Topic -> m ResponseReceived) -> Tp.TopicRef -> m ResponseReceived
withTopic act tref = runQuery (Gh.getBy tref) >>= maybe notFound act
    where notFound = respondNotFound (TopicNotFound tref)
    
-- ** topic permissions 

-- | only run the inner query if the passed in caller data conforms to the
-- permissions of the topic; e.g.if membership is required, only run the inner query if the
-- authenticated caller can read the topic, etc
topicQueryGuard :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> m ResponseReceived -> m ResponseReceived
topicQueryGuard maybeCaller topicData inner 
    | Tp.membershipRequired topicData = evalCont $ do
        caller <- cont reauth
        memberMode <- cont $ withMemberAuth topicData caller (QueryDenied NotMember)
        let canQuery = Tm.mmRead memberMode || Tp.isUserCreator caller topicData
        return $ if canQuery then inner else handleAccessDenied (QueryDenied ReadDenied)
    | Tp.authRequired topicData = reauth (const inner)
    | otherwise = inner
    where
    reauth = callerReauth maybeCaller 

-- | respond with only the fields of the topic that the current call is
-- allowed to see, based on the topic mode and the authentication and
-- membership of the caller.
getTopicForCall :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> m ResponseReceived
getTopicForCall maybeCaller topicData
    | Tp.membershipRequired topicData = runMaybeT membershipRequiredResponse >>= maybe respondCensored return
    | Tp.authRequired topicData = maybe respondCensored (const respondFull) maybeCaller
    | otherwise = respondFull
    where
    respondCensored = respondOk $ Json $ Tp.CensoredTopic topicData
    respondFull = respondOk $ Json topicData
    membershipRequiredResponse = do
        caller <- MaybeT $ tryGetAuth maybeCaller
        void $ MaybeT $ findMemberUser topicData caller
        lift respondFull

-- | allow the call to receive a particular field of the topic based on the
-- same censorship rules as 'getTopicForCall'
getTopicFieldForCall :: (MonadRespond m, MonadChatless m, ToJSON v) => Maybe Ur.User -> Tp.Topic -> (Tp.Topic -> v) -> m ResponseReceived
getTopicFieldForCall maybeCaller topicData f
    | Tp.membershipRequired topicData = reauth $ \caller -> withMemberAuth topicData caller (QueryDenied NotMember) (const respondField)
    | Tp.authRequired topicData = reauth (const respondField)
    | otherwise = respondField
    where
    respondField = respondOk $ Json $ f topicData
    reauth = callerReauth maybeCaller 

-- ** Topic member queries 

-- | find a member by topic ref and user ref 
findMember :: MonadChatless m => Tp.TopicRef -> Ur.UserRef -> m (Maybe Tm.Member)
findMember tr ur = runQuery $ Gh.getBy (Tm.TargetMemberKey tr ur) 

-- | find a member by topic and user ref
findMemberRef :: MonadChatless m => Tp.Topic -> Ur.UserRef -> m (Maybe Tm.Member)
findMemberRef = findMember . Tp.getRefFromTopic

-- | find a member by topic and user 
findMemberUser :: MonadChatless m => Tp.Topic -> Ur.User -> m (Maybe Tm.Member)
findMemberUser tp us = findMember (Tp.getRefFromTopic tp) (Ur.getRefFromUser us)

-- | only run the inner route if a membership can be found for the caller;
-- otherwise fail authorization with the given ReportableError.
withMemberAuth :: (MonadChatless m, MonadRespond m, ReportableError e) => Tp.Topic -> Ur.User -> e -> (Tm.MemberMode -> m ResponseReceived) -> m ResponseReceived
withMemberAuth topic user err inner = findMemberUser topic user >>= \mMember -> authorizeE (toAuth mMember) inner
    where
    toAuth = maybe (Left err) (Right . Tm.memberMode)

-- | groundhog query that finds all the members of the topic
topicMembersQuery :: (Functor m, Gh.PersistBackend m) => Tp.TopicRef -> m [Tm.MemberPartial]
topicMembersQuery tr = fmap (uncurry Tm.MemberPartial) <$> Gh.project (Tm.MemberUserField, Tm.MemberModeField) (Tm.MemberTopicField Gh.==. tr)

-- | get a list of topic members
listTopicMembers :: MonadChatless m => Tp.Topic -> m [Tm.MemberPartial]
listTopicMembers = runQuery . topicMembersQuery . Tp.getRefFromTopic

-- | respond to a call with the list of topic members if the permissions
-- allow the caller to read. see 'topicQueryGuard'
listTopicMembersForCall :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> m ResponseReceived
listTopicMembersForCall maybeCaller topicData = topicQueryGuard maybeCaller topicData $ listTopicMembers topicData >>= respondOk . Json

-- | respond to a call with a topic member's mode if the caller is allowed
-- to. see 'topicQueryGuard
getTopicMemberForCall :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> Ur.UserRef -> m ResponseReceived
getTopicMemberForCall maybeCaller topicData memberRef = topicQueryGuard maybeCaller topicData $ 
    findMemberRef topicData memberRef >>= maybe (respondNotFound $ MemberNotFound (Tp.getRefFromTopic topicData) memberRef) (respondOk . Json . Tm.memberMode)

