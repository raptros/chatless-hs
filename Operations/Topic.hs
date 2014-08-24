{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses, ConstraintKinds #-}
module Operations.Topic where

import Safe (headMay)
import Control.Monad (when, (>=>))
import Control.Monad.Catch (throwM)
import qualified Database.Groundhog as Gh
import Control.Applicative ((<$>))
import Data.Maybe (isJust)
import Data.Either (isLeft)
import Data.Bool (bool)

import Model.StorableJson (StorableJson)
import Model.IDGen (genRandom)
import Model.ID (MessageId)
import qualified Model.User as Ur
import qualified Model.Topic as Tp
import qualified Model.TopicMember as Tm
import qualified Model.Message as Msg

import Operations.Base (runOp, CatchDbConn, OpError(..), OpType(..), getOrThrow, throwEitherConst, (.*))
import qualified Operations.Internal.Topic as T

getTopic :: CatchDbConn m cm conn => Tp.TopicRef -> m (Either OpError Tp.Topic)
getTopic = runOp . T.opGetTopic

setTopicMode :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Tp.TopicModeUpdate -> m (Either OpError Msg.Message)
setTopicMode caller tr tmu = runOp $ do
    topic <- T.testTopicPerm caller tr (const Tm.mmSetMode) SetTopicMode
    let newMode = Tp.resolveTopicModeUpdate (Tp.topicMode topic) tmu
    Gh.update [Tp.TopicModeField Gh.=. newMode] $ Tp.TopicCoord Gh.==. tr
    T.opCreateMessage caller tr $ Msg.MsgTopicModeChanged newMode

listMembers :: (CatchDbConn m cm conn, Functor m) => Ur.UserRef -> Tp.TopicRef -> m (Either OpError [Tm.MemberPartial])
listMembers callerRef tr = runOp $ do
    T.testTopicPerm_ callerRef tr (const Tm.mmRead) ReadTopic
    res <- Gh.project (Tm.MemberUserField, Tm.MemberModeField) (Tm.MemberTopicField Gh.==. tr)
    return $ uncurry Tm.MemberPartial <$> res

getMember :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> m (Either OpError Tm.MemberMode)
getMember callerRef tr ur = runOp $ do
    T.testTopicPerm_ callerRef tr (const Tm.mmRead) ReadTopic
    res <- Gh.getBy (Tm.TargetMemberKey tr ur) 
    getOrThrow (MemberNotFound tr ur) (Tm.memberMode <$> res)

setMemberMode :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> Tm.MemberModeUpdate -> m (Either OpError Msg.Message)
setMemberMode callerRef tr targetUser mmu = runOp $ do
    T.testTopicPerm_ callerRef tr (const Tm.mmSetMember) SetMemberMode
    targetMember <- Gh.getBy (Tm.TargetMemberKey tr targetUser) >>= getOrThrow (MemberNotFound tr targetUser)
    let newMode = Tm.resolveMemberModeUpdate (Tm.memberMode targetMember) mmu
    Gh.update [Tm.MemberModeField Gh.=. newMode] $ (Tm.MemberTopicField Gh.==. tr) Gh.&&. (Tm.MemberUserField Gh.==. targetUser)
    T.opCreateMessage callerRef tr $ Msg.MsgMemberModeChanged targetUser newMode

createTopic :: (CatchDbConn m cm conn) => Ur.UserRef -> Tp.TopicCreate -> m (Either OpError Tp.Topic)
createTopic ur tc = runOp $ do 
    tid <- maybe genRandom return $ Tp.createId tc
    let tr = Tp.fromUserRef tid ur
        newTopic = Tp.initializeTopic ur tid tc
        givenId = isJust $ Tp.createId tc
    --create a topic and insert the creator
    tcRes <- Gh.insertByAll newTopic
    when (isLeft tcRes) $ throwM $ checkedFailure givenId tr
    Gh.insert $ Tm.Member tr ur Tm.modeCreator
    T.opCreateMessage_ ur tr $ Msg.MsgUserJoined Tm.modeCreator
    return newTopic
    where checkedFailure True = IdInUse
          checkedFailure False = GenerateIdFailed ur . (:[]) . Tp.topicRefId

inviteToTopic :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Ur.UserRef -> StorableJson -> m (Either OpError Msg.Message)
inviteToTopic caller tr ur body = runOp $ do
    newMember <- T.testTopicPerm caller tr (const Tm.mmInvite) InviteUser >>= insertNewMember
    user <- Gh.getBy ur >>= getOrThrow (UserNotFound ur)
    let newMode = Tm.memberMode newMember
        invitesRef = Tp.fromUserRef (Ur.userInvite user) ur
    T.testJoinTopicPerm_ caller invitesRef Tm.canSend SendMessage
    T.opCreateMessage_ caller invitesRef $ Msg.MsgInvitation tr newMode body
    T.opCreateMessage caller tr $ Msg.MsgInvitedUser ur invitesRef newMode
    where insertNewMember = T.opInsertMember ur tr . Tm.invitedMode . Tp.topicMode >=> throwEitherConst (AlreadyMember tr ur)
        
joinTopic :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> m (Either OpError Tm.MemberMode)
joinTopic = runOp .* T.opJoinTopic

sendMessage :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> StorableJson -> m (Either OpError Msg.Message)
sendMessage caller tr body = runOp $ do
    T.testTopicPerm_ caller tr Tm.canSend SendMessage 
    T.opCreateMessage caller tr $ Msg.MsgPosted body

getFromEnd :: CatchDbConn m cm conn => Bool -> Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Msg.Message])
getFromEnd forward caller tr n = runOp $ do
    T.testTopicPerm_ caller tr (const Tm.mmRead) ReadTopic
    let dir = bool Gh.Desc Gh.Asc forward
    handles <- Gh.select $ (Msg.MhTopicField Gh.==. tr) `Gh.orderBy` [dir Gh.AutoKeyField] `Gh.limitTo` n
    mapM T.opLoadHandle handles

getFirst :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Msg.Message])
getFirst = getFromEnd True 

getLast :: CatchDbConn m cm conn => Ur.UserRef -> Tp.TopicRef -> Int -> m (Either OpError [Msg.Message])
getLast = getFromEnd False

getFromId :: CatchDbConn m cm conn => Bool -> Bool -> Ur.UserRef -> Tp.TopicRef -> MessageId -> Int -> m (Either OpError [Msg.Message])
getFromId forward inclusive caller tr mid n = runOp $ do
    T.testTopicPerm_ caller tr (const Tm.mmRead) ReadTopic
    targetMessageQuery >>= extractAutoKey >>= listQuery >>= mapM T.opLoadHandle 
    where -- | targetMessageQuery projects out the auto key field of the desired message
          targetMessageQuery = Gh.project Gh.AutoKeyField $ (Msg.MhTopicField Gh.==. tr) Gh.&&. (Msg.MhIdField Gh.==. mid) 
          -- | extractAutoKey determines if a message was found
          extractAutoKey = getOrThrow (MessageNotFound $ Msg.MessageCoordKey tr mid) . headMay
          dir = bool Gh.Desc Gh.Asc forward
          selectComp True True = (Gh.>=.)
          selectComp True False = (Gh.>.)
          selectComp False True = (Gh.<=.)
          selectComp False False = (Gh.<.)
          comp = selectComp forward inclusive
          -- | gets messages adjacent to the target
          listQuery k = Gh.select $ ((Msg.MhTopicField Gh.==. tr) Gh.&&. (Gh.AutoKeyField `comp` k)) `Gh.orderBy` [dir Gh.AutoKeyField] `Gh.limitTo` n


