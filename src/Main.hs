{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
module Main where

import qualified Data.Text as T
import Data.Pool (Pool)
import Database.Groundhog
import Database.Groundhog.Sqlite
--import Database.Groundhog.Postgresql
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Trans.Control (MonadTransControl, StT, liftWith, restoreT, defaultLiftWith, defaultRestoreT, MonadBaseControl, StM, liftBaseWith, defaultLiftBaseWith, restoreM, defaultRestoreM, ComposeSt)
import Database.Groundhog.Core (ConnectionManager(..))
import Control.Monad.Logger (NoLoggingT)
import Control.Monad
import Model.User
import Model.Topic
import Model.TopicMember
import Model.Message
import Model.ID
import Model.StorableJson

import Api
import Web.Respond.DefaultServer
import System.Log.FastLogger

import Data.Maybe

main :: IO ()
main = do
    logger <- newStdoutLoggerSet defaultBufSize 
    pool <- setup
    let cl = CLConfig {
        _clcServerId = ServerId "local",
        _clcDb = pool
    }
    runWaiApp 3000 logger (apiApplication cl)

setup :: IO (Pool CLDb)
setup = do
    pool <- createSqlitePool "fakery.db" 1
    runChatlessMigrate pool
    (flip runDbConn) pool $ insertFakeUsers "local" ["user0", "user1", "user2"]
    return pool

insertFakeUsers :: PersistBackend m => T.Text -> [T.Text] -> m ()
insertFakeUsers = mapM_ . insertFakeUser

insertFakeUser :: PersistBackend m => T.Text -> T.Text -> m ()
insertFakeUser sid uid = do
    let userRef = UserCoordKey (ServerId sid) (UserId uid)
        fakeUser = makeFakeUser sid uid
        aboutTopic = Topic (ServerId sid) (UserId uid) (userAbout fakeUser) uid storableEmpty aboutTopicMode
        aboutMember = Member (extractUnique aboutTopic) userRef modeCreator
        inviteTopic = Topic (ServerId sid) (UserId uid) (userInvite fakeUser) uid storableEmpty inviteTopicMode
        inviteMember = Member (extractUnique inviteTopic) userRef modeCreator
    u <- getBy userRef
    when (isNothing u) $ do
        insert fakeUser
        insert aboutTopic
        insert aboutMember
        insert inviteTopic
        insert inviteMember


makeFakeUser :: T.Text -> T.Text -> User
makeFakeUser sid uid = User (ServerId sid) (UserId uid) (TopicId "about") (TopicId "invite")

runChatlessMigrate :: (ConnectionManager cm conn, MonadBaseControl IO m, MonadIO m, PersistBackend (DbPersist conn (NoLoggingT m))) => cm -> m ()
runChatlessMigrate = runDbConn $ runMigration defaultMigrationLogger $ do
    migrate (undefined :: User)
    migrate (undefined :: Topic)
    migrate (undefined :: Member)
    messageMigration
