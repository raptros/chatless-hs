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
    where
    failure = throwError (LoadMessageFailed (Msg.MessageCoordKey tr mid))
    success = return . Msg.Message tr mid sender

queryMessageHandle :: (MonadError QueryFailure m, Gh.PersistBackend m) => Msg.MessageRef -> m Msg.MsgHandle
queryMessageHandle mr = Gh.getBy mr >>= getOrThrow (MessageNotFound mr)

queryMessagesFromEnd :: (Gh.PersistBackend m, MonadError QueryFailure m) => Direction -> Tp.TopicRef -> Int -> m [Msg.Message]
queryMessagesFromEnd dir tr count = listQ >>= mapM queryLoadHandle
    where
    listQ = Gh.select $ (Msg.MhTopicField Gh.==. tr) `Gh.orderBy` [dirQ Gh.AutoKeyField] `Gh.limitTo` count 
    dirQ = direction Gh.Asc Gh.Desc dir

runQueryMessagesFromEnd :: MonadChatless m => Direction -> Tp.TopicRef -> Int -> m (Either QueryFailure [Msg.Message])
runQueryMessagesFromEnd dir tr count = runExceptT $ runQuery $ queryMessagesFromEnd dir tr count

messageQuery :: (MonadChatless m, MonadRespond m) => (Tp.TopicRef -> Int -> m (Either QueryFailure [Msg.Message])) -> Tp.Topic -> Maybe Natural -> m ResponseReceived
messageQuery queryRunner topicData mCount = queryRunner (Tp.getRefFromTopic topicData) (getCountDefault mCount) >>= either (respondReportError notFound404 []) (respondOk . Json)

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
    dirQ = direction Gh.Asc Gh.Desc dir
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

