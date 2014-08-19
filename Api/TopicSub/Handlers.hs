{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Api.TopicSub.Handlers where

import Api.Utils
import Api.TopicSub.Data
import Api.Root
import Api.RootUtils
import Operations
import Yesod.Core
import Safe

import Model.ID
import Model.User
import Model.Message
import Model.Topic
import Model.TopicMember
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Generic (runDb)
import Database.Groundhog.Sqlite
import Database.Groundhog.Postgresql
import Database.Groundhog.Core (ConnectionManager(..))
import Network.HTTP.Types
import Data.Pool
import Data.Maybe
import Data.Text.Encoding (decodeUtf8)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except

type TopicHandler a = HandlerT TopicSub Handler a
--type ChatlessTopicHandler a = TopicHandler Chatless a

getTopicR :: TopicHandler Value
getTopicR = do
    tc <- readTopicRef
    mTopic <- lift $ runDb $ getBy tc
    maybe notFound returnJson mTopic

readTopicRef :: TopicHandler TopicRef
readTopicRef = ask >>= getTopicRef

getTopicRef :: TopicSub -> TopicHandler TopicRef
--getTopicRef (MeTopicSub sid tid) = (\uid -> TopicCoordKey sid uid tid) <$> extractUserId
--getTopicRef (MeAboutTopicSub sid) = pickTopicFromUser userAbout sid <$> loadMe sid
--getTopicRef (MeInvitesTopicSub sid) = pickTopicFromUser userInvite sid <$> loadMe sid
--each of these refers to the current session's user
getTopicRef (MeTopicSub tid) =    lift $ fromUserRef tid <$> getCaller
getTopicRef (MeAboutTopicSub) =   lift $ pickTopicFromUser userAbout <$> loadMe
getTopicRef (MeInvitesTopicSub) = lift $ pickTopicFromUser userInvite <$> loadMe
--these refer to a local user
getTopicRef (LocalUserTopicSub uid tid) =    lift $ (\sid -> TopicCoordKey sid uid tid) <$> reader localServer
getTopicRef (LocalUserAboutTopicSub uid) =   lift $ pickTopicFromUser userAbout <$> getLocalUser uid
getTopicRef (LocalUserInvitesTopicSub uid) = lift $ pickTopicFromUser userInvite <$>  getLocalUser uid
--these refer to any user
getTopicRef (AnyUserAboutTopicSub sid uid) =   lift $ pickTopicFromUser userAbout <$> getAnyUser sid uid
getTopicRef (AnyUserInvitesTopicSub sid uid) = lift $ pickTopicFromUser userInvite <$> getAnyUser sid uid
getTopicRef (AnyUserTopicSub sid uid tid) =  return $ TopicCoordKey sid uid tid


pickTopicFromUser :: (User -> TopicId) -> User -> TopicRef
pickTopicFromUser c u = TopicCoordKey (userServer u) (userId u) (c u)

--getTopic = do
    --
getMembersR :: TopicHandler Value
getMembersR = do
    callerRef <- lift getCaller
    tr <- readTopicRef
    membersRes <- lift $ listMembers callerRef tr
    respondOpResult membersRes


getMemberR :: ServerId -> UserId -> TopicHandler Value
getMemberR sid uid = do 
    caller <- lift getCaller
    tr <- readTopicRef
    let ur = UserCoordKey sid uid
    lift (getMember caller tr ur) >>= respondOpResult
        
-- action: set the mode of an existing member.
putMemberR :: ServerId -> UserId -> TopicHandler Value
putMemberR sid uid = do
    let targetUser = UserCoordKey sid uid
    newMode <- requireJsonBody :: TopicHandler MemberModeUpdate
    tr <- readTopicRef
    callerRef <- lift getCaller
    res <- lift $ setMemberMode callerRef tr targetUser newMode 
    respondOpResult res

-- action: invite a new member to the topic
postMemberR :: ServerId -> UserId -> TopicHandler Value
postMemberR sid uid = sendResponseStatus notImplemented501 $ reasonObject "not_implemented" []


--listTopics :: (HasConn m cm conn, PersistBackend (DbPersist conn m)) => ServerId -> UserId -> m [TopicRef]
{-
getCallerEffectiveMode :: (MonadHandler m, MonadReader Chatless m, HasConn m cm conn, PersistBackend (DbPersist conn m)) => Topic -> m MemberMode
getCallerEffectiveMode topic = do
    sid <- reader localServer
    cid <- extractUserId
    if cid == topicUser topic then return modeCreator else findMember
    where findMember = runDb $ getBy $ TargetMemberKey (extractUnique topic) (UserCoordKey sid cid) >>= fromMaybe (nonMemberMode $ mode topic)
          -}

getLocalMemberR :: UserId -> TopicHandler Value
getLocalMemberR = useForLocal getMemberR

putLocalMemberR :: UserId -> TopicHandler Value
putLocalMemberR = useForLocal putMemberR

postLocalMemberR :: UserId -> TopicHandler Value
postLocalMemberR = useForLocal postMemberR

useForLocal :: (ServerId -> UserId -> TopicHandler a) -> UserId -> TopicHandler a
useForLocal h = (lift (reader localServer) >>=) . flip h



{-
getLocalUserTopicR :: UserId -> TopicId -> Handler Value
getLocalUserTopicR uid tid = do
    lserv <- reader localServer
    mTopic <- runDb $ getBy $ TopicCoordKey lserv uid tid
    maybe notFound returnJson mTopic
-}
