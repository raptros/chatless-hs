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
    tr <- readTopicRef
    --todo: permissions
    members <- lift $ runDb (project (MemberUserField, MemberModeField) $ MemberTopicField ==. tr)
    returnJson $ (uncurry subsetAsJson) <$> members

getMemberR :: ServerId -> UserId -> TopicHandler Value
getMemberR sid uid = do
    tr <- readTopicRef
    --todo: permissions
    let ur = UserCoordKey sid uid
        tm = TargetMemberKey tr ur
    mMember <- lift $ runDb $ getBy tm
    maybe notFound (returnJson . memberMode) mMember

-- action: set the mode of an existing member.
putMemberR :: ServerId -> UserId -> TopicHandler Value
putMemberR sid uid = do
    let targetUser = UserCoordKey sid uid
    newMode <- requireJsonBody :: TopicHandler MemberMode
    tr <- readTopicRef
    callerRef <- lift $ getCaller
    res <- lift $ runDb $ runExceptT $ setMemberModeOp targetUser newMode tr callerRef
    --returnJson $ Member tr targetUser newMode
    either (uncurry sendResponseStatus) (const $ returnJson $ Member tr targetUser newMode) res

setMemberModeOp :: (PersistBackend m, Functor m, Monad m) => UserRef -> MemberMode -> TopicRef -> UserRef -> ExceptT (Status, Value) m ()
setMemberModeOp targetUser newMode tr callerRef = do
        topic <- (lift $ getBy tr) >>= maybe (throwError (notFound404, reasonObject "not_found" ["topic" .= tr])) return
        em <- lift $ getMemberEffectiveMode callerRef topic
        --todo all reason strings need to be pulled out so somewhere (for consistent spelling)
        unless (mmSetMember em) $ throwError (forbidden403, reasonObject "impermissible" ["operation" .= ("set_member_mode" :: T.Text) ])
        mTargetMember <- lift $ getBy $ TargetMemberKey tr targetUser
        when (isNothing mTargetMember) $ throwError (notFound404, reasonObject "not_found" ["topic" .= tr, "member" .= targetUser])
        lift $ update [MemberModeField =. newMode] $ (MemberTopicField ==. tr) &&. (MemberUserField ==. targetUser)
        --todo send a message
        return ()

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
