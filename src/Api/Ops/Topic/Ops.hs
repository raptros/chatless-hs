{- |

the actual topic operations
-}
{-# LANGUAGE FlexibleContexts #-}
module Api.Ops.Topic.Ops where

import qualified Data.Text as T

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Trans.Control (MonadBaseControl)

import qualified Database.Groundhog as Gh

import Chatless.Model.StorableJson
import qualified Chatless.Model.User as Ur
import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.TopicMember as Tm
import qualified Chatless.Model.Message as Msg

import Api.Monad
import Api.Ops.Base
import Api.Ops.Topic.Base
import Api.Ops.Topic.Message
import Api.Ops.Topic.Parts

sendMessage :: (MonadChatless m, MonadBaseControl IO m) => Ur.User -> Tp.Topic -> StorableJson -> m TopicOpResult
sendMessage caller topic body = tryTopicOp $ operateTopicSimple caller topic SendMessage Msg.MsgPosted (Just body) (const $ return ())

changeBanner :: (MonadChatless m, MonadBaseControl IO m) => Ur.User -> Tp.Topic -> T.Text -> m TopicOpResult
changeBanner caller topic newBanner = tryTopicOp $ operateTopicIfChanged caller topic SetBanner Msg.MsgBannerChanged (Tp.topicBanner topic) newBanner go
    where 
    go = Gh.update [Tp.TopicBannerField Gh.=. newBanner] $ Tp.TopicCoord Gh.==. Tp.getRefFromTopic topic

changeInfo :: (MonadChatless m, MonadBaseControl IO m) => Ur.User -> Tp.Topic -> StorableJson -> m TopicOpResult
changeInfo caller topic newInfo = tryTopicOp $ operateTopicIfChanged caller topic SetInfo Msg.MsgInfoChanged (Tp.topicInfo topic) newInfo go
    where
    go = Gh.update [Tp.TopicInfoField Gh.=. newInfo] $ Tp.TopicCoord Gh.==. Tp.getRefFromTopic topic

changeTopicMode :: (MonadChatless m, MonadBaseControl IO m) => Ur.User -> Tp.Topic -> Tp.TopicModeUpdate -> m TopicOpResult 
changeTopicMode caller topic tmu = tryTopicOp $ operateTopicSimple caller topic SetTopicMode Msg.MsgTopicModeChanged changed go
    where
    changed = Tp.resolveTopicModeUpdateMay (Tp.topicMode topic) tmu
    go newMode = Gh.update [Tp.TopicModeField Gh.=. newMode] $ Tp.TopicCoord Gh.==. Tp.getRefFromTopic topic

changeMemberMode :: (MonadChatless m, MonadBaseControl IO m) => Ur.UserRef -> Ur.User -> Tp.Topic -> Tm.MemberModeUpdate -> m TopicOpResult
changeMemberMode targetUserRef caller topic mmu = tryTopicOp $ operateTopic caller topic SetMemberMode mkMessage changed go
    where
    mkMessage = Msg.MsgMemberModeChanged targetUserRef
    changed = flip Tm.resolveMemberModeUpdateMay mmu <$> opGetTargetMemberMode TargetNotMember SetMemberMode topic targetUserRef
    go newMemberMode = Gh.update [Tm.MemberModeField Gh.=. newMemberMode] $ Tm.TargetMember Gh.==. Tm.TargetMemberKey (Tp.getRefFromTopic topic) targetUserRef

sendInvite :: (MonadChatless m, MonadBaseControl IO m) => Ur.UserRef -> Ur.User -> Tp.Topic -> StorableJson -> m TopicOpResult
sendInvite targetUserRef caller topic inviteBody = tryTopicOp $ do
    topicOpPermitGuard SendInvite caller topic
    -- get the target user 
    let tr = Tp.getRefFromTopic topic
        opFailed failure = TopicOpFailed SendInvite failure tr
    targetUser <- Gh.getBy targetUserRef >>= throwOrReturn (opFailed (NoSuchUser targetUserRef))
    -- get the target user's invite topic
    let inviteTopicRef = Tp.userInviteTopicRef targetUser
    targetInviteTopic <- Gh.getBy inviteTopicRef >>= throwOrReturn (opFailed (InviteTopicMissing inviteTopicRef))
    -- add the target user to this topic
    let mkInvitedMessage = Msg.MsgInvitedUser targetUserRef inviteTopicRef
    invitedMemberMode <- opAddMember SendInvite Tm.invitedMode mkInvitedMessage caller topic targetUserRef
    -- perform a send message operation within the target's invite topic
    topicOpEnsuredPermitGuard SendMessage caller targetInviteTopic
    opWriteMessage SendMessage caller targetInviteTopic (Msg.MsgInvitation tr invitedMemberMode inviteBody)

joinTopic :: (MonadChatless m, MonadBaseControl IO m) => Ur.User -> Tp.Topic -> m TopicOpResult
joinTopic caller topic = tryTopicOp $ void $ opAddMember JoinTopic Tm.joinerMode Msg.MsgUserJoined caller topic (Ur.getRefFromUser caller)



