{-# LANGUAGE OverloadedStrings, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Api.TopicSub.Handlers where

import Control.Applicative ((<$>))
import Control.Monad.Reader (reader, ask)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (Value)
import Yesod.Core.Handler (HandlerT)
import Yesod.Core.Json (requireJsonBody)

import Model.StorableJson (StorableJson)
import Model.ID (ServerId, UserId, TopicId, MessageId)
import Model.User (User, userServer, userId, userAbout, userInvite, Key(UserCoordKey))
import Model.Topic (topicMode, TopicRef, fromUserRef, Key(TopicCoordKey))
import Model.TopicMember (MemberModeUpdate)

import qualified Operations as Op

import Api.Utils (respondOpResult)
import Api.Root (Handler, localServer)
import Api.RootUtils (getCaller, loadMe, getLocalUser, getAnyUser)
import Api.TopicSub.Data (TopicSub(..))


type TopicHandler a = HandlerT TopicSub Handler a

getTopicR :: TopicHandler Value
getTopicR = readTopicRef >>= lift . Op.getTopic >>= respondOpResult

getTopicModeR :: TopicHandler Value
getTopicModeR = readTopicRef >>= lift . Op.getTopic >>= respondOpResult . fmap topicMode

putTopicModeR :: TopicHandler Value
putTopicModeR = do
    callerRef <- lift getCaller
    tr <- readTopicRef
    newMode <- requireJsonBody
    lift (Op.setTopicMode callerRef tr newMode) >>= respondOpResult

pickTopicFromUser :: (User -> TopicId) -> User -> TopicRef
pickTopicFromUser c u = TopicCoordKey (userServer u) (userId u) (c u)

getMembersR :: TopicHandler Value
getMembersR = do
    callerRef <- lift getCaller
    tr <- readTopicRef
    lift (Op.listMembers callerRef tr) >>= respondOpResult

getMemberR :: ServerId -> UserId -> TopicHandler Value
getMemberR sid uid = do 
    caller <- lift getCaller
    tr <- readTopicRef
    let ur = UserCoordKey sid uid
    lift (Op.getMember caller tr ur) >>= respondOpResult
        
-- action: set the mode of an existing member.
putMemberR :: ServerId -> UserId -> TopicHandler Value
putMemberR sid uid = do
    let targetUser = UserCoordKey sid uid
    newMode <- requireJsonBody :: TopicHandler MemberModeUpdate
    tr <- readTopicRef
    callerRef <- lift getCaller
    lift (Op.setMemberMode callerRef tr targetUser newMode) >>= respondOpResult

-- action: invite a new member to the topic
postMemberR :: ServerId -> UserId -> TopicHandler Value
postMemberR sid uid = do
    caller <- lift getCaller
    let targetUser = UserCoordKey sid uid
    tr <- readTopicRef
    invite <- requireJsonBody :: TopicHandler StorableJson
    lift (Op.inviteToTopic caller tr (UserCoordKey sid uid) invite) >>= respondOpResult

getLocalMemberR :: UserId -> TopicHandler Value
getLocalMemberR = useForLocal getMemberR

putLocalMemberR :: UserId -> TopicHandler Value
putLocalMemberR = useForLocal putMemberR

postLocalMemberR :: UserId -> TopicHandler Value
postLocalMemberR = useForLocal postMemberR

useForLocal :: (ServerId -> UserId -> TopicHandler a) -> UserId -> TopicHandler a
useForLocal h = (lift (reader localServer) >>=) . flip h

getMsgR :: TopicHandler Value
getMsgR = getMsgFirstR 1

postMsgR :: TopicHandler Value
postMsgR = do
    caller <- lift getCaller
    tr <- readTopicRef
    body <- requireJsonBody :: TopicHandler StorableJson
    lift (Op.sendMessage caller tr body) >>= respondOpResult

getMsgFirst1R :: TopicHandler Value
getMsgFirst1R = getMsgFirstR 1

getMsgFirstR :: Int -> TopicHandler Value
getMsgFirstR = getMsgFromEndR True

getMsgLast1R :: TopicHandler Value
getMsgLast1R = getMsgLastR 1

getMsgLastR :: Int -> TopicHandler Value
getMsgLastR = getMsgFromEndR False

getMsgFromEndR :: Bool -> Int -> TopicHandler Value
getMsgFromEndR forward count = do 
    caller <- lift getCaller
    tr <- readTopicRef
    lift (Op.getFromEnd forward caller tr count) >>= respondOpResult

getMsgBefore1R :: MessageId -> TopicHandler Value
getMsgBefore1R = flip getMsgBeforeR 1

getMsgBeforeR :: MessageId -> Int -> TopicHandler Value
getMsgBeforeR = getMsgByIdR False False 

getMsgAfter1R :: MessageId -> TopicHandler Value
getMsgAfter1R = flip getMsgAfterR 1

getMsgAfterR :: MessageId -> Int -> TopicHandler Value
getMsgAfterR = getMsgByIdR True False

getMsgAt1R :: MessageId -> TopicHandler Value
getMsgAt1R = flip getMsgAtR 1

getMsgAtR :: MessageId -> Int -> TopicHandler Value
getMsgAtR = getMsgByIdR False True

getMsgFrom1R :: MessageId -> TopicHandler Value
getMsgFrom1R = flip getMsgFromR 1

getMsgFromR :: MessageId -> Int -> TopicHandler Value
getMsgFromR = getMsgByIdR True True

getMsgByIdR :: Bool -> Bool -> MessageId -> Int -> TopicHandler Value
getMsgByIdR forward inclusive id count = do
    caller <- lift getCaller
    tr <- readTopicRef
    lift (Op.getFromId forward inclusive caller tr id count) >>= respondOpResult

readTopicRef :: TopicHandler TopicRef
readTopicRef = ask >>= getTopicRef

getTopicRef :: TopicSub -> TopicHandler TopicRef
--each of these refers to the current session's user
getTopicRef (MeTopicSub tid) =    lift $ fromUserRef tid <$> getCaller
getTopicRef (MeAboutTopicSub) =   lift $ pickTopicFromUser userAbout <$> loadMe
getTopicRef (MeInviteTopicSub) = lift $ pickTopicFromUser userInvite <$> loadMe
--these refer to a local user
getTopicRef (LocalUserTopicSub uid tid) =    lift $ (\sid -> TopicCoordKey sid uid tid) <$> reader localServer
getTopicRef (LocalUserAboutTopicSub uid) =   lift $ pickTopicFromUser userAbout <$> getLocalUser uid
getTopicRef (LocalUserInviteTopicSub uid) = lift $ pickTopicFromUser userInvite <$>  getLocalUser uid
--these refer to any user
getTopicRef (AnyUserAboutTopicSub sid uid) =   lift $ pickTopicFromUser userAbout <$> getAnyUser sid uid
getTopicRef (AnyUserInviteTopicSub sid uid) = lift $ pickTopicFromUser userInvite <$> getAnyUser sid uid
getTopicRef (AnyUserTopicSub sid uid tid) =  return $ TopicCoordKey sid uid tid

