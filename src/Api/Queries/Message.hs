{-|
Description: message queries

queries for messages
-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Queries.Message where

import Network.Wai
import Network.HTTP.Types.Status
import Web.Respond
import qualified Database.Groundhog as Gh
import Control.Monad.Logger ()
import Control.Monad.Except
import Safe (headMay)

import Chatless.Model.ID
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.Message as Msg
import Api.Monad
import Api.Queries.Base
import Api.Queries.Topic

-- * the actual queries

justMessage :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> m ResponseReceived
justMessage maybeCaller topicData mid = topicQueryGuard maybeCaller topicData $ runMessageJust mr >>= respondReportEither notFound404 Json
    where
    mr = Msg.MessageCoordKey (Tp.getRefFromTopic topicData) mid

-- ** queries from either end

messagesFirst :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messagesFirst = messagesAtEnd Forwards

messagesLast :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messagesLast = messagesAtEnd Backwards

-- ** queries around an ID
messagesBefore :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesBefore = messagesOnId Backwards Exclusive

messagesAfter :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesAfter = messagesOnId Forwards Exclusive

messagesFrom :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesFrom = messagesOnId Forwards Inclusive

messagesAt :: (MonadRespond m, MonadChatless m) => Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesAt = messagesOnId Backwards Inclusive


-- * query setup

data Inclusion = Inclusive | Exclusive

-- | the first one is if yes, second is if not
inclusion :: a -> a -> Inclusion -> a
inclusion v _ Inclusive = v
inclusion _ v Exclusive = v

data Direction = Forwards | Backwards

direction :: a -> a -> Direction -> a
direction v _ Forwards = v
direction _ v Backwards = v

respondReportEither :: (MonadRespond m, ReportableError e, ToResponseBody b) => Status -> (a -> b) -> Either e a -> m ResponseReceived
respondReportEither errStatus toOkBody = either (respondReportError errStatus []) (respondOk . toOkBody)

-- ** tools for obtaining a message

queryLoadHandle :: (MonadError QueryFailure m, Gh.PersistBackend m) => Msg.MsgHandle -> m Msg.Message
queryLoadHandle (Msg.MsgHandle tr mid sender time k) = Gh.get k >>= maybe failure success
    where
    failure = throwError (LoadMessageFailed (Msg.MessageCoordKey tr mid))
    success = return . Msg.Message tr mid sender time

queryMessageHandle :: (MonadError QueryFailure m, Gh.PersistBackend m) => Msg.MessageRef -> m Msg.MsgHandle
queryMessageHandle mr = Gh.getBy mr >>= getOrThrow (MessageNotFound mr)

runMessageJust :: (MonadChatless m) => Msg.MessageRef -> m (Either QueryFailure Msg.Message)
runMessageJust = runExceptT . runQuery . (queryMessageHandle >=> queryLoadHandle)
    
queryMessageJust :: (Gh.PersistBackend m, MonadError QueryFailure m) => Msg.MessageRef -> m Msg.Message
queryMessageJust = queryMessageHandle >=> queryLoadHandle

-- ** getting multiple messages

-- | applies a query runner and produces a response
messageQuery :: (MonadChatless m, MonadRespond m) => (Tp.TopicRef -> Int -> m (Either QueryFailure [Msg.Message])) -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messageQuery queryRunner topicData mCount = queryRunner (Tp.getRefFromTopic topicData) (getCountDefault mCount) >>= respondReportEither notFound404 Json

-- *** querying messages from an end

-- | responds with the messages at a particular end
messagesAtEnd :: (MonadRespond m, MonadChatless m) => Direction -> Maybe Ur.User -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messagesAtEnd dir maybeCaller topicData = topicQueryGuard maybeCaller topicData . messageQuery (runQueryMessagesFromEnd dir) topicData

-- | query runner for getting messages at and end
runQueryMessagesFromEnd :: MonadChatless m => Direction -> Tp.TopicRef -> Int -> m (Either QueryFailure [Msg.Message])
runQueryMessagesFromEnd dir tr count = runExceptT . runQuery $ queryMessagesFromEnd dir tr count

queryMessagesFromEnd :: (Gh.PersistBackend m, MonadError QueryFailure m) => Direction -> Tp.TopicRef -> Int -> m [Msg.Message]
queryMessagesFromEnd dir tr count = listQ >>= mapM queryLoadHandle
    where
    listQ = Gh.select $ (Msg.MhTopicField Gh.==. tr) `Gh.orderBy` [dirQ Gh.AutoKeyField] `Gh.limitTo` count 
    dirQ = direction Gh.Asc Gh.Desc dir

-- ** querying messages around an ID

-- | responds with the messages around a specific message.
messagesOnId :: (MonadRespond m, MonadChatless m) => Direction -> Inclusion -> Maybe Ur.User -> Tp.Topic -> MessageId -> Maybe Natural -> m ResponseReceived
messagesOnId dir inc maybeCaller topicData mid = topicQueryGuard maybeCaller topicData . messageQuery (runQueryMessagesOnId dir inc mid) topicData 

-- | runs a query to find messages around an id
runQueryMessagesOnId :: MonadChatless m => Direction -> Inclusion -> MessageId -> Tp.TopicRef -> Int -> m (Either QueryFailure [Msg.Message])
runQueryMessagesOnId dir include mid tr count = runExceptT . runQuery $ queryMessagesOnId dir include mid tr count

-- | queries for messages around an ID
queryMessagesOnId :: (Gh.PersistBackend m, MonadError QueryFailure m) => Direction -> Inclusion -> MessageId -> Tp.TopicRef -> Int -> m [Msg.Message]
queryMessagesOnId dir include mid tr count = targetMessageQuery >>= extractAutoKey >>= listQuery >>= mapM queryLoadHandle
    where 
    -- | targetMessageQuery projects out the auto key field of the desired message
    targetMessageQuery = Gh.project Gh.AutoKeyField $ (Msg.MhTopicField Gh.==. tr) Gh.&&. (Msg.MhIdField Gh.==. mid) 
    -- | extractAutoKey determines if a message was found
    extractAutoKey = getOrThrow (MessageNotFound $ Msg.MessageCoordKey tr mid) . headMay
    dirQ = direction Gh.Asc Gh.Desc dir
    selectComp Forwards Inclusive = (Gh.>=.)
    selectComp Forwards Exclusive = (Gh.>.)
    selectComp Backwards Inclusive = (Gh.<=.)
    selectComp Backwards Exclusive = (Gh.<.)
    comp = selectComp dir include
    -- | gets messages adjacent to the target
    listQuery k = Gh.select $ ((Msg.MhTopicField Gh.==. tr) Gh.&&. (Gh.AutoKeyField `comp` k)) `Gh.orderBy` [dirQ Gh.AutoKeyField] `Gh.limitTo` count
