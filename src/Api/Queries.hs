{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Queries where

import Control.Applicative ((<$>))
import Model.ID
import qualified Model.User as Ur
import qualified Model.Topic as Tp
import qualified Model.TopicMember as Tm
import qualified Model.Message as Msg
import Network.Wai
import Data.Aeson
import Network.HTTP.Types.Status
import Web.Respond
import qualified Database.Groundhog as Gh
import qualified Data.Text as T
import Web.Respond.HListUtils ()
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Except
import Control.Monad (void)
import Safe (headMay)

import Control.Lens ((<&>))

import Api.Monad
import Api.Auth

-- * query utils.
 
-- | constraint synonym
type MonadControlIO m = (MonadBaseControl IO m, MonadIO m, MonadLogger m)

-- | why a query was denied
data QueryDenyReason =
    NotMember |
    ReadDenied

-- | text representations for query denials
queryDenyReasonText :: QueryDenyReason -> T.Text
queryDenyReasonText NotMember = "not_member"
queryDenyReasonText ReadDenied = "read_denied"

-- | reportable errors for query failures.
data QueryFailure =
    UserNotFound Ur.UserRef |
    TopicNotFound Tp.TopicRef |
    MemberNotFound Tp.TopicRef Ur.UserRef |
    QueryDenied QueryDenyReason |
    LoadMessageFailed Msg.MessageRef |
    MessageNotFound Msg.MessageRef

-- | the string not_found
textNotFound :: T.Text
textNotFound = "not_found"

instance ReportableError QueryFailure where
    toErrorReport (UserNotFound ur) = errorReportWithDetails textNotFound $ object ["user" .= ur]
    toErrorReport (TopicNotFound tr) = errorReportWithDetails textNotFound $ object ["topic" .= tr]
    toErrorReport (MemberNotFound tr ur) = errorReportWithDetails textNotFound $ object ["topic" .= tr, "user" .= ur]
    toErrorReport (QueryDenied qdr) = simpleErrorReport (queryDenyReasonText qdr) 
    toErrorReport (LoadMessageFailed mr) = errorReportWithDetails "load_message_failed" $ object ["message" .= mr]
    toErrorReport (MessageNotFound mr) = errorReportWithDetails textNotFound $ object ["message" .= mr]

-- * user queries 

-- | run an action on a user ... if that user can be found in the db
withUser :: (MonadChatless m, MonadRespond m) => (Ur.User -> m ResponseReceived) -> Ur.UserRef -> m ResponseReceived
withUser act uref = runQuery (Gh.getBy uref) >>= maybe notFound act
    where notFound = respond $ ResponseError notFound404 (UserNotFound uref)

-- | list out the topics a user has created
getUserTopics :: MonadChatless m => Ur.User -> m [Tp.TopicRef]
getUserTopics = runQuery . userTopicsQuery

-- | the groundhog query.
userTopicsQuery :: Gh.PersistBackend m => Ur.User -> m [Tp.TopicRef]
userTopicsQuery user = Gh.project Tp.TopicCoord $ (Tp.TopicServerField Gh.==. Ur.userServer user) Gh.&&. (Tp.TopicUserField Gh.==. Ur.userId user)

-- * Topic queries 

-- | run the action given the topic, if the topic can be found
withTopic :: (MonadChatless m, MonadRespond m) => (Tp.Topic -> m ResponseReceived) -> Tp.TopicRef -> m ResponseReceived
withTopic act tref = runQuery (Gh.getBy tref) >>= maybe notFound act
    where notFound = respond $ ResponseError notFound404 (TopicNotFound tref)
    
-- ** topic permissions 

-- | only run the inner query if the passed in caller data conforms to the
-- permissions of the topic; e.g.if membership is required, only run the inner query if the
-- authenticated caller can read the topic, etc
topicQueryGuard :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> m ResponseReceived -> m ResponseReceived
topicQueryGuard maybeCaller topicData inner 
    | Tp.membershipRequired topicData = reauth $ \caller -> 
        withMemberAuth topicData caller (QueryDenied NotMember) $ \memberMode ->
            if Tm.mmRead memberMode || Tp.isUserCreator caller topicData then inner else handleDenied $ QueryDenied ReadDenied
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
    respondCensored = respond $ OkJson $ Tp.CensoredTopic topicData
    respondFull = respond $ OkJson topicData
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
    respondField = respond $ OkJson $ f topicData
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
withMemberAuth topic user err = authorizeE (findMemberUser topic user <&> maybe (Left err) (Right . Tm.memberMode))

-- | groundhog query that finds all the members of the topic
topicMembersQuery :: (Functor m, Gh.PersistBackend m) => Tp.TopicRef -> m [Tm.MemberPartial]
topicMembersQuery tr = fmap (uncurry Tm.MemberPartial) <$> Gh.project (Tm.MemberUserField, Tm.MemberModeField) (Tm.MemberTopicField Gh.==. tr)

-- | get a list of topic members
listTopicMembers :: MonadChatless m => Tp.Topic -> m [Tm.MemberPartial]
listTopicMembers = runQuery . topicMembersQuery . Tp.getRefFromTopic

-- * message queries
-- | respond to a call with the list of topic members if the permissions
-- allow the caller to read. see 'topicQueryGuard'
listTopicMembersForCall :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> m ResponseReceived
listTopicMembersForCall maybeCaller topicData = topicQueryGuard maybeCaller topicData $ listTopicMembers topicData >>= respond . OkJson

-- | respond to a call with a topic member's mode if the caller is allowed
-- to. see 'topicQueryGuard
getTopicMemberForCall :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> Ur.UserRef -> m ResponseReceived
getTopicMemberForCall maybeCaller topicData memberRef = topicQueryGuard maybeCaller topicData $ 
    findMemberRef topicData memberRef >>= maybeNotFound (MemberNotFound (Tp.getRefFromTopic topicData) memberRef) (respond . OkJson . Tm.memberMode)

-- * message queries

data Inclusion = Inclusive | Exclusive

-- | the first one is if yes, second is if not
inclusion :: a -> a -> Inclusion -> a
inclusion v _ Inclusive = v
inclusion _ v Exclusive = v

data Direction = Forwards | Backwards

direction :: a -> a -> Direction -> a
direction v _ Forwards = v
direction _ v Backwards = v

queryLoadHandle :: (MonadError QueryFailure m, Gh.PersistBackend m) => Msg.MsgHandle -> m Msg.Message
queryLoadHandle (Msg.MsgHandle tr mid sender k) = Gh.get k >>= maybe failure success
    where failure = throwError (LoadMessageFailed (Msg.MessageCoordKey tr mid))
          success = return . Msg.Message tr mid sender

queryMessageHandle :: (MonadError QueryFailure m, Gh.PersistBackend m) => Msg.MessageRef -> m Msg.MsgHandle
queryMessageHandle mr = Gh.getBy mr >>= getOrThrow (MessageNotFound mr)

getOrThrow :: MonadError e m => e -> Maybe a -> m a
getOrThrow err = maybe (throwError err) return

natToInt :: Natural -> Int
natToInt (Natural integer) = fromInteger integer

getCountDefault :: Maybe Natural -> Int
getCountDefault = maybe 1 natToInt

-- absolutely disgusting
instance MonadError QueryFailure m => MonadError QueryFailure (Gh.DbPersist conn m) where
    throwError = lift . throwError
    catchError act h = Gh.DbPersist $ catchError (Gh.unDbPersist act) (Gh.unDbPersist . h)

instance MonadError QueryFailure m => MonadError QueryFailure (NoLoggingT m) where
    throwError = lift . throwError
    catchError act h = NoLoggingT $ catchError (runNoLoggingT act) (runNoLoggingT . h)

queryMessagesFromEnd :: (Gh.PersistBackend m, MonadError QueryFailure m) => Direction -> Tp.TopicRef -> Int -> m [Msg.Message]
queryMessagesFromEnd dir tr count = listQ >>= mapM queryLoadHandle
    where
    listQ = Gh.select $ (Msg.MhTopicField Gh.==. tr) `Gh.orderBy` [dirQ Gh.AutoKeyField] `Gh.limitTo` count 
    dirQ = direction Gh.Desc Gh.Asc dir

runQueryMessagesFromEnd :: MonadChatless m => Direction -> Tp.TopicRef -> Int -> m (Either QueryFailure [Msg.Message])
runQueryMessagesFromEnd dir tr count = runExceptT $ runQuery $ queryMessagesFromEnd dir tr count

messageQuery :: (MonadChatless m, MonadRespond m) => (Tp.TopicRef -> Int -> m (Either QueryFailure [Msg.Message])) -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messageQuery queryRunner topicData mCount = queryRunner (Tp.getRefFromTopic topicData) (getCountDefault mCount) >>= either (respond . ResponseError notFound404) (respond . OkJson)

messagesAtEnd :: (MonadRespond m, MonadChatless m) => Direction -> Maybe Ur.User -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messagesAtEnd dir maybeCaller topicData = topicQueryGuard maybeCaller topicData . messageQuery (runQueryMessagesFromEnd dir) topicData

messagesFirst :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messagesFirst = messagesAtEnd Forwards

messagesLast :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messagesLast = messagesAtEnd Backwards

queryMessagesOnId :: (Gh.PersistBackend m, MonadError QueryFailure m) => Direction -> Inclusion -> MessageId -> Tp.TopicRef -> Int -> m [Msg.Message]
queryMessagesOnId dir include mid tr count = targetMessageQuery >>= extractAutoKey >>= listQuery >>= mapM queryLoadHandle
    where 
    -- | targetMessageQuery projects out the auto key field of the desired message
    targetMessageQuery = Gh.project Gh.AutoKeyField $ (Msg.MhTopicField Gh.==. tr) Gh.&&. (Msg.MhIdField Gh.==. mid) 
    -- | extractAutoKey determines if a message was found
    extractAutoKey = getOrThrow (MessageNotFound $ Msg.MessageCoordKey tr mid) . headMay
    dirQ = direction Gh.Desc Gh.Asc dir
    selectComp Forwards Inclusive = (Gh.>=.)
    selectComp Forwards Exclusive = (Gh.>.)
    selectComp Backwards Inclusive = (Gh.<=.)
    selectComp Backwards Exclusive = (Gh.<.)
    comp = selectComp dir include
    -- | gets messages adjacent to the target
    listQuery k = Gh.select $ ((Msg.MhTopicField Gh.==. tr) Gh.&&. (Gh.AutoKeyField `comp` k)) `Gh.orderBy` [dirQ Gh.AutoKeyField] `Gh.limitTo` count

runQueryMessagesOnId :: MonadChatless m => Direction -> Inclusion -> MessageId -> Tp.TopicRef -> Int -> m (Either QueryFailure [Msg.Message])
runQueryMessagesOnId dir include mid tr count = runExceptT $ runQuery $ queryMessagesOnId dir include mid tr count

messagesOnId :: (MonadRespond m, MonadChatless m) => Direction -> Inclusion -> Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesOnId dir inc maybeCaller topicData mid = topicQueryGuard maybeCaller topicData . messageQuery (runQueryMessagesOnId dir inc mid) topicData 

messagesBefore :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesBefore = messagesOnId Backwards Exclusive

messagesAfter :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesAfter = messagesOnId Forwards Exclusive

messagesFrom :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesFrom = messagesOnId Forwards Inclusive

messagesAt :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesAt = messagesOnId Backwards Inclusive
