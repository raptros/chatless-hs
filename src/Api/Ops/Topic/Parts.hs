{- |

utilties for topic operations
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Ops.Topic.Parts where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Base (MonadBase)
import Control.Exception.Lifted (throwIO)

import qualified Database.Groundhog as Gh

import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
import qualified Chatless.Model.Message as Msg
import qualified Data.Foldable as Fld

import Api.Ops.Base
import Api.Ops.Topic.Base
import Api.Ops.Topic.Message

-- | given a function that gets a target user's mode from the topic, throws
-- an exception if a user does not have permission to perform an operation
-- in the topic.
topicOpModeGuard :: (MonadBase IO m, Functor m) => (TopicOp -> Tp.Topic -> Ur.User -> m Tm.MemberMode) -> TopicOp -> Ur.User -> Tp.Topic -> m ()
topicOpModeGuard opGetMode op user topic = opGetMode op topic user >>= \mmode -> unless (topicOpAllowed op tmode mmode) (throwIO err)
    where
    tref = Tp.getRefFromTopic topic
    tmode = Tp.topicMode topic
    err = TopicOpFailed op NotPermitted tref

-- | throws an exception if the user is not a member or does not have
-- permission to perform the operation.
--
-- this is just 'topicOpModeGuard' using 'opGetEffectiveMode'
topicOpPermitGuard :: (MonadBase IO m, Functor m, Gh.PersistBackend m) => TopicOp -> Ur.User -> Tp.Topic -> m ()
topicOpPermitGuard = topicOpModeGuard opGetEffectiveMode

-- | gets the effective member mode of the caller. if the user is the
-- creator, returns 'modeCreator'. otherwise, throws a 'TopicOpFailed' with
-- 'NotMember'.
opGetEffectiveMode :: (MonadBase IO m, Functor m, Gh.PersistBackend m) => TopicOp -> Tp.Topic -> Ur.User -> m Tm.MemberMode
opGetEffectiveMode op topic user
    | Tp.isUserCreator user topic = return Tm.modeCreator
    | otherwise = opGetTargetMemberMode (const CallerNotMember) op topic (Ur.getRefFromUser user)

-- | gets the mode of a member of a topic, throwing a TopicOpFailed exception if the
-- target is not a member
opGetTargetMemberMode :: (MonadBase IO m, Functor m, Gh.PersistBackend m) => (Ur.UserRef -> TopicOpFailureReason) -> TopicOp -> Tp.Topic -> Ur.UserRef -> m Tm.MemberMode
opGetTargetMemberMode fReason op topic targetRef = Gh.getBy (Tm.TargetMemberKey tr targetRef) >>= fmap Tm.memberMode . throwOrReturn err
    where
    tr = Tp.getRefFromTopic topic
    err = TopicOpFailed op (fReason targetRef) tr

-- | attempt to add a 
opAddMember :: MonadMessages m => TopicOp -> (Tp.TopicMode -> Tm.MemberMode) -> (Tm.MemberMode -> Msg.MsgContent) -> Ur.User -> Tp.Topic -> Ur.UserRef -> m Tm.MemberMode
opAddMember op getMMode mkMsg caller topic targetRef = opInsertMember op getMMode mkMsg caller topic targetRef >>= either (const alreadyMember) return
    where
    alreadyMember = throwIO $ TopicOpFailed op (TargetAlreadyMember targetRef) (Tp.getRefFromTopic topic)

topicOpEnsuredPermitGuard :: MonadMessages m => TopicOp -> Ur.User -> Tp.Topic -> m ()
topicOpEnsuredPermitGuard = topicOpModeGuard opGetEnsuredMode

-- | works similarly to 'opGetEffectiveMode', except that it calls
-- 'opEnsureMember' in the case where the target user is not the creator.
opGetEnsuredMode :: MonadMessages m => TopicOp -> Tp.Topic -> Ur.User -> m Tm.MemberMode
opGetEnsuredMode op topic user
    | Tp.isUserCreator user topic = return Tm.modeCreator
    | otherwise = opEnsureMember op user topic

-- | 
opEnsureMember :: MonadMessages m => TopicOp -> Ur.User -> Tp.Topic -> m Tm.MemberMode
opEnsureMember op caller topic = either id id  <$> 
    opInsertMember op Tm.joinerMode Msg.MsgUserJoined caller topic (Ur.getRefFromUser caller) 

-- | attempts to add a member to the topic. if the user is already
-- a member, fails with the existing membership's mode. otherwise, succeeds
-- with the created member's mode. in this case, it will write a message
-- describing the new member into the topic.
opInsertMember :: MonadMessages m => TopicOp -> (Tp.TopicMode -> Tm.MemberMode) -> (Tm.MemberMode -> Msg.MsgContent) -> Ur.User -> Tp.Topic -> Ur.UserRef -> m (Either Tm.MemberMode Tm.MemberMode)
opInsertMember op getMMode mkMsg caller topic targetRef = Gh.getBy (Tm.TargetMemberKey tr targetRef) >>= maybe inner (return . Left . Tm.memberMode)
    where
    tr = Tp.getRefFromTopic topic
    newMode = getMMode (Tp.topicMode topic)
    inner = do
        Gh.insert_ $ Tm.Member tr targetRef newMode 
        opWriteMessage op caller topic $ mkMsg newMode
        return $ Right newMode

-- * constructing operations

-- | topic operations seem to have a basic structure - determine if there
-- is an actual change, if there is, perform it and then send a message
-- describing the change
operateTopic :: MonadMessages m 
             => Ur.User -- ^ caller
             -> Tp.Topic -- ^ topic to operate in (lol)
             -> TopicOp -- ^ the operation that is being performed
             -> (v -> Msg.MsgContent) -- ^ what message to send for the change
             -> m (Maybe v) -- ^ if the value is being changed, should produce the new one
             -> (v -> m ()) -- ^ action to write the new value
             -> m () -- ^ can be run by 'tryTopicOp'
operateTopic caller topic op mkMessageBody changeCheck performUpdate = do
    topicOpPermitGuard op caller topic 
    mNewVal <- changeCheck
    -- forM_ over the Maybe runs the inner action only if there is
    -- Just a value
    Fld.forM_ mNewVal $ \newVal -> do
        performUpdate newVal
        opWriteMessage op caller topic $ mkMessageBody newVal

-- | simplifies 'operateTopic' by taking Maybe a new value directly,
-- instead of in a monadic action
operateTopicSimple :: MonadMessages m
                   => Ur.User 
                   -> Tp.Topic 
                   -> TopicOp 
                   -> (v -> Msg.MsgContent) 
                   -> Maybe v  -- ^ simplified value change test
                   -> (v -> m ()) 
                   -> m ()
operateTopicSimple caller topic op mkMsg = operateTopic caller topic op mkMsg . return 

-- | simplifies 'operateTopic' by taking an old value and a new value and
-- running the action etc only if the two values are not equal.
operateTopicIfChanged :: (Eq v, MonadMessages m)
                      => Ur.User 
                      -> Tp.Topic 
                      -> TopicOp 
                      -> (v -> Msg.MsgContent) -- ^ message to send only after action is performed
                      -> v -- ^ old value
                      -> v -- ^ new value
                      -> m () -- ^ action to perform
                      -> m ()
operateTopicIfChanged caller topic op mkMessage old new = operateTopicSimple caller topic op mkMessage (passNewIfChanged old new) . const

