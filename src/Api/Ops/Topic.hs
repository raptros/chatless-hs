{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Api.Ops.Topic where

--import Control.Monad.Except
--import Safe (headMay)
import Data.Either (isRight)
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Typeable
import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (mapReaderT)
import Control.Monad.Catch
import Control.Monad.Logger (NoLoggingT)

import qualified Database.Groundhog as Gh
--import qualified Database.Groundhog.Core as Gh
import Network.HTTP.Types.Status
import Network.Wai
import Web.Respond
import Data.Bool (bool)

import Chatless.Model.ID
import Chatless.Model.IDGen (genRandom)
import Chatless.Model.StorableJson
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
import qualified Chatless.Model.Message as Msg
import qualified Data.Foldable as Fld

import Api.Config
import Api.Monad

-- * types and functions for describing operation results and errors

-- | the operation types
data TopicOp =
    SetBanner |
    SetInfo |
    SetTopicMode |
    SetMemberMode |
    SendMessage |
    SendInvite |
    JoinTopic
    deriving (Eq, Show)

-- | strings representing the operations
topicOpText :: TopicOp -> T.Text
topicOpText SetBanner = "set banner"
topicOpText SetInfo = "set info"
topicOpText SetTopicMode = "set topic mode"
topicOpText SetMemberMode = "set member mode"
topicOpText SendMessage = "send message"
topicOpText SendInvite = "send invite"
topicOpText JoinTopic = "join topic"

-- | produce a predicate determining if a user is allowed to perform the
-- specified operation
topicOpAllowed :: TopicOp -> Tp.TopicMode -> Tm.MemberMode -> Bool
topicOpAllowed SetBanner = const Tm.mmSetBanner
topicOpAllowed SetInfo = const Tm.mmSetInfo
topicOpAllowed SetTopicMode = const Tm.mmSetMode
topicOpAllowed SetMemberMode = const Tm.mmSetMember
topicOpAllowed SendMessage = Tm.canSend
topicOpAllowed SendInvite = const Tm.mmInvite
topicOpAllowed JoinTopic = const (const True)

-- | various ways a topic operation can fail
data TopicOpFailureReason  = 
    CallerNotMember | 
    TargetNotMember (Ur.UserRef) |
    NotPermitted |
    MsgIdInUse MessageId |
    MsgIdGenFailed [MessageId] |
    TargetAlreadyMember (Ur.UserRef) |
    NoSuchUser (Ur.UserRef) |
    InviteTopicMissing (Tp.TopicRef)
    deriving (Eq, Show)

-- | convert the failure with the topic ref into an error report
tofrPrepare :: TopicOpFailureReason -> (T.Text, [Pair])
tofrPrepare CallerNotMember            = ("not member", [])
tofrPrepare (TargetNotMember uref)     = ("not member", ["user" .= uref])
tofrPrepare NotPermitted               = ("not permitted", [])
tofrPrepare (MsgIdInUse mid)           = ("message id in use", ["mid" .= mid])
tofrPrepare (MsgIdGenFailed mids)      = ("message id generation failed", ["tried" .= mids])
tofrPrepare (TargetAlreadyMember uref) = ("already member", ["user" .= uref])
tofrPrepare (NoSuchUser uref)          = ("no such user", ["user" .= uref])
tofrPrepare (InviteTopicMissing tref)  = ("invite topic missing", ["targetRef" .= tref])


-- | status codes appropriate for each failure type
topicOpFailureStatus :: TopicOpFailureReason-> Status
topicOpFailureStatus CallerNotMember         = forbidden403
topicOpFailureStatus (TargetNotMember _)     = notFound404
topicOpFailureStatus NotPermitted            = forbidden403
topicOpFailureStatus (MsgIdInUse _)          = badRequest400
topicOpFailureStatus (MsgIdGenFailed _)      = internalServerError500
topicOpFailureStatus (TargetAlreadyMember _) = badRequest400
topicOpFailureStatus (NoSuchUser _)          = notFound404
topicOpFailureStatus (InviteTopicMissing _)  = internalServerError500

-- | wrapper around a failure type and a topic ref; it can be thrown and it
-- can be reported as a response body
data TopicOpFailed = TopicOpFailed {
    tofOperation :: TopicOp,
    tofReason :: TopicOpFailureReason,
    tofTopicRef :: Tp.TopicRef
} deriving (Eq, Typeable, Show)

instance Exception TopicOpFailed

topicOpFailedErrorReport :: TopicOpFailed -> ErrorReport
topicOpFailedErrorReport (TopicOpFailed op fReason tref) = errorReportWithDetails reasonText $ object (moreDetails ++ details)
    where
    details = ["topic" .= tref, "op" .= topicOpText op]
    (reasonText, moreDetails) = tofrPrepare fReason

instance ReportableError TopicOpFailed where
    reportError = reportAsErrorReport topicOpFailedErrorReport

topicOpFailedStatus :: TopicOpFailed -> Status
topicOpFailedStatus = topicOpFailureStatus . tofReason

respondTopicOpFailed :: MonadRespond m => TopicOpFailed -> m ResponseReceived
respondTopicOpFailed failed = respondReportError (topicOpFailedStatus failed) [] failed

type TopicOpResult = Either TopicOpFailed [Msg.MessageRef]

respondMessagesCreated :: MonadRespond m => [Msg.MessageRef] -> m ResponseReceived
respondMessagesCreated msgs
    | null msgs = respondEmptyBody noContent204 []
    | otherwise = respondOk $ Json msgs 

respondTopicOpResult :: MonadRespond m => TopicOpResult -> m ResponseReceived
respondTopicOpResult = either respondTopicOpFailed respondMessagesCreated

-- * the operations
sendMessage :: (MonadChatless m, MonadCatch m) => Ur.User -> Tp.Topic -> StorableJson -> m TopicOpResult
sendMessage caller topic body = tryTopicOp $ operateTopicSimple caller topic SendMessage Msg.MsgPosted (Just body) (const $ return ())

changeBanner :: (MonadChatless m, MonadCatch m) => Ur.User -> Tp.Topic -> T.Text -> m TopicOpResult
changeBanner caller topic newBanner = tryTopicOp $ operateTopicIfChanged caller topic SetBanner Msg.MsgBannerChanged (Tp.topicBanner topic) newBanner go
    where 
    go = Gh.update [Tp.TopicBannerField Gh.=. newBanner] $ Tp.TopicCoord Gh.==. Tp.getRefFromTopic topic

changeInfo :: (MonadChatless m, MonadCatch m) => Ur.User -> Tp.Topic -> StorableJson -> m TopicOpResult
changeInfo caller topic newInfo = tryTopicOp $ operateTopicIfChanged caller topic SetInfo Msg.MsgInfoChanged (Tp.topicInfo topic) newInfo go
    where
    go = Gh.update [Tp.TopicInfoField Gh.=. newInfo] $ Tp.TopicCoord Gh.==. Tp.getRefFromTopic topic

changeTopicMode :: (MonadChatless m, MonadCatch m) => Ur.User -> Tp.Topic -> Tp.TopicModeUpdate -> m TopicOpResult 
changeTopicMode caller topic tmu = tryTopicOp $ operateTopicSimple caller topic SetTopicMode Msg.MsgTopicModeChanged changed go
    where
    changed = Tp.resolveTopicModeUpdateMay (Tp.topicMode topic) tmu
    go newMode = Gh.update [Tp.TopicModeField Gh.=. newMode] $ Tp.TopicCoord Gh.==. Tp.getRefFromTopic topic

changeMemberMode :: (MonadChatless m, MonadCatch m) => Ur.UserRef -> Ur.User -> Tp.Topic -> Tm.MemberModeUpdate -> m TopicOpResult
changeMemberMode targetUserRef caller topic mmu = tryTopicOp $ operateTopic caller topic SetMemberMode mkMessage changed go
    where
    mkMessage = Msg.MsgMemberModeChanged targetUserRef
    changed = flip Tm.resolveMemberModeUpdateMay mmu <$> opGetTargetMemberMode TargetNotMember SetMemberMode topic targetUserRef
    go newMemberMode = Gh.update [Tm.MemberModeField Gh.=. newMemberMode] $ Tm.TargetMember Gh.==. Tm.TargetMemberKey (Tp.getRefFromTopic topic) targetUserRef

sendInvite :: (MonadChatless m, MonadCatch m) => Ur.UserRef -> Ur.User -> Tp.Topic -> StorableJson -> m TopicOpResult
sendInvite targetUserRef caller topic inviteBody = tryTopicOp $ do
    lift $ topicOpPermitGuard SendInvite caller topic
    -- get the target user 
    let tr = Tp.getRefFromTopic topic
        opFailed failure = TopicOpFailed SendInvite failure tr
    targetUser <- lift $ Gh.getBy targetUserRef >>= throwOrReturn (opFailed (NoSuchUser targetUserRef))
    -- get the target user's invite topic
    let inviteTopicRef = Tp.userInviteTopicRef targetUser
    targetInviteTopic <- lift $ Gh.getBy inviteTopicRef >>= throwOrReturn (opFailed (InviteTopicMissing inviteTopicRef))
    -- add the target user to this topic
    let mkInvitedMessage = Msg.MsgInvitedUser targetUserRef inviteTopicRef
    invitedMemberMode <- opAddMember SendInvite Tm.invitedMode mkInvitedMessage caller topic targetUserRef
    -- perform a send message operation within the target's invite topic
    topicOpEnsuredPermitGuard SendMessage caller targetInviteTopic
    opWriteMessage SendMessage caller targetInviteTopic (Msg.MsgInvitation tr invitedMemberMode inviteBody)

joinTopic :: (MonadChatless m, MonadCatch m) => Ur.User -> Tp.Topic -> m TopicOpResult
joinTopic caller topic = tryTopicOp $ void $ opAddMember JoinTopic Tm.joinerMode Msg.MsgUserJoined caller topic (Ur.getRefFromUser caller)


-- ** tools for defining topic operations

-- | topic operations need to record all the messages they will insert
type TopicOperationT m a = Gh.DbPersist CLDb (NoLoggingT (WriterT [Msg.MessageRef] m)) a

type MonadMessages m = (Functor m, MonadThrow m, MonadIO m, Gh.PersistBackend m, MonadWriter [Msg.MessageRef] m)

-- | runs the topic operaton and catches any TopicOpFailed exceptions
tryTopicOp :: (MonadChatless m, MonadCatch m) => TopicOperationT m () -> m TopicOpResult
tryTopicOp = try . execWriterT . runTransaction

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
             -> WriterT [Msg.MessageRef] m () -- ^ can be run by 'tryTopicOp'
operateTopic caller topic op mkMessageBody changeCheck performUpdate = do
    lift $ topicOpPermitGuard op caller topic 
    mNewVal <- lift changeCheck
    -- forM_ over the Maybe runs the inner action only if there is
    -- Just a value
    Fld.forM_ mNewVal $ \newVal -> do
        lift $ performUpdate newVal
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

-- ** topic utilities

-- | old, new, produce the new only if not the old
passNewIfChanged :: Eq a => a -> a -> Maybe a
passNewIfChanged old new = bool Nothing (Just new) (old /= new)
                   
-- | throws an exception when the Maybe is Nothing
throwOrReturn :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwOrReturn = flip maybe return . throwM

-- | gets the effective member mode of the caller. if the user is the
-- creator, returns 'modeCreator'. otherwise, throws a 'TopicOpFailed' with
-- 'NotMember'.
opGetEffectiveMode :: (MonadThrow m, Functor m, Gh.PersistBackend m) => TopicOp -> Tp.Topic -> Ur.User -> m Tm.MemberMode
opGetEffectiveMode op topic user
    | Tp.isUserCreator user topic = return Tm.modeCreator
    | otherwise = opGetTargetMemberMode (const CallerNotMember) op topic (Ur.getRefFromUser user)

-- | throws an exception if the user is not a member or does not have
-- permission to perform the operation.
topicOpPermitGuard :: (MonadThrow m, Functor m, Gh.PersistBackend m) => TopicOp -> Ur.User -> Tp.Topic -> m ()
topicOpPermitGuard op user topic = opGetEffectiveMode op topic user >>= \mmode -> unless (topicOpAllowed op tmode mmode) (throwM err)
    where
    tref = Tp.getRefFromTopic topic
    tmode = Tp.topicMode topic
    err = TopicOpFailed op NotPermitted tref

-- | gets the mode of a member of a topic, throwing a TopicOpFailed exception if the
-- target is not a member
opGetTargetMemberMode :: (MonadThrow m, Functor m, Gh.PersistBackend m) => (Ur.UserRef -> TopicOpFailureReason) -> TopicOp -> Tp.Topic -> Ur.UserRef -> m Tm.MemberMode
opGetTargetMemberMode fReason op topic targetRef = Gh.getBy (Tm.TargetMemberKey tr targetRef) >>= fmap Tm.memberMode . throwOrReturn err
    where
    tr = Tp.getRefFromTopic topic
    err = TopicOpFailed op (fReason targetRef) tr

-- | retrys the action up to n times.
retryNTimes :: (Alternative f) => Int -> f a -> f a
retryNTimes n act = Fld.asum $ replicate n act

-- | attempt to add a 
opAddMember :: MonadMessages m => TopicOp -> (Tp.TopicMode -> Tm.MemberMode) -> (Tm.MemberMode -> Msg.MsgContent) -> Ur.User -> Tp.Topic -> Ur.UserRef -> m Tm.MemberMode
opAddMember op getMMode mkMsg caller topic targetRef = opInsertMember op getMMode mkMsg caller topic targetRef >>= either (const $ alreadyMember) return
    where
    alreadyMember = throwM $ TopicOpFailed op (TargetAlreadyMember targetRef) (Tp.getRefFromTopic topic)

opInsertMember :: MonadMessages m => TopicOp -> (Tp.TopicMode -> Tm.MemberMode) -> (Tm.MemberMode -> Msg.MsgContent) -> Ur.User -> Tp.Topic -> Ur.UserRef -> m (Either Tm.MemberMode Tm.MemberMode)
opInsertMember op getMMode mkMsg caller topic targetRef = lift (Gh.getBy targetMemberRef) >>= maybe inner (return . Left . Tm.memberMode)
    where
    tr = Tp.getRefFromTopic topic
    targetMemberRef = Tm.TargetMemberKey tr targetRef
    newMode = getMMode (Tp.topicMode topic)
    inner = do
        lift $ Gh.insert_ $ Tm.Member tr targetRef newMode 
        opWriteMessage op caller topic $ mkMsg newMode
        return $ Right newMode

opEnsureMember :: MonadMessages m => TopicOp -> Ur.User -> Tp.Topic -> WriterT [Msg.MessageRef] m Tm.MemberMode
opEnsureMember op caller topic = combine <$> go
    where
    combine = either id id
    go = opInsertMember op Tm.joinerMode Msg.MsgUserJoined caller topic (Ur.getRefFromUser caller) 

opGetEnsuredMode :: MonadMessages m => TopicOp -> Tp.Topic -> Ur.User -> m Tm.MemberMode
opGetEnsuredMode op topic user
    | Tp.isUserCreator user topic = return Tm.modeCreator
    | otherwise = opEnsureMember op user topic

topicOpEnsuredPermitGuard :: MonadMessages m => TopicOp -> Ur.User -> Tp.Topic -> m ()
topicOpEnsuredPermitGuard op user topic = opGetEnsuredMode op topic user >>= \mmode -> unless (topicOpAllowed op tmode mmode) (throwM err)
    where
    tref = Tp.getRefFromTopic topic
    tmode = Tp.topicMode topic
    err = TopicOpFailed op NotPermitted tref


-- ** writing messages

opWriteMessage :: MonadMessages m => TopicOp -> Ur.User -> Tp.Topic -> Msg.MsgContent -> m ()
opWriteMessage op sender topic content = do
    msg <- lift $ opAttemptMsgCreate op sender topic content
    tell [Msg.getRefFromMessage msg]

-- | how many times 'opAttemptMsgCreate' should attempt to generate an id
-- and save the message
msgCreateMaxAttempts :: Int
msgCreateMaxAttempts = 3

opAttemptMsgCreate :: (Functor m, MonadThrow m, MonadIO m, Gh.PersistBackend m) => TopicOp -> Ur.User -> Tp.Topic -> Msg.MsgContent -> m Msg.Message
opAttemptMsgCreate op sender topic content = runAttempts >>= throwOrReturn <$> onFail . snd <*> fst
    where
    onFail tried = TopicOpFailed op (MsgIdGenFailed tried) (Tp.getRefFromTopic topic)
    runAttempts = runWriterT . runMaybeT $ retryNTimes msgCreateMaxAttempts attempt
    attempt = do
        mid <- genRandom
        tell [mid]
        MaybeT $ lift $ opStoreMessage sender topic content mid

opStoreMessage :: Gh.PersistBackend m => Ur.User -> Tp.Topic -> Msg.MsgContent -> MessageId -> m (Maybe Msg.Message)
opStoreMessage sender topic content mid = do
    contentId <- Gh.insert content
    res <- Gh.insertByAll $ Msg.MsgHandle topicRef mid senderRef contentId
    return $ mayWhen (Msg.Message topicRef mid senderRef content) (isRight res)
    where
    topicRef = Tp.getRefFromTopic topic
    senderRef = Ur.getRefFromUser sender

mapDbPersist :: (m a -> n b) -> Gh.DbPersist conn m a -> Gh.DbPersist conn n b
mapDbPersist f m = Gh.DbPersist $ mapReaderT f $ Gh.unDbPersist m

-- absolutely disgusting
instance MonadThrow m => MonadThrow (Gh.DbPersist conn m) where
    throwM = lift . throwM

instance MonadWriter w m => MonadWriter w (Gh.DbPersist conn m) where
    writer = lift . writer
    tell = lift . tell
    listen = mapDbPersist listen
    pass = mapDbPersist pass 
