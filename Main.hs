{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
module Main where

import Data.Aeson
import qualified Data.Text as T
import Yesod.Core
import Database.Groundhog
import Database.Groundhog.Sqlite
import Database.Groundhog.Postgresql
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
import Api.Root
import Data.Maybe

main :: IO ()
main = do
    pool <- setup
    warp 3000 $ Chatless {
        backendConn = pool,
        localServer = ServerId "local"
    }

setup = do
    pool <- createSqlitePool "fakery.db" 1
    runChatlessMigrate pool
    (flip runDbConn) pool $ insertFakeUsers "local" ["user0", "user1"]
    return pool

insertFakeUsers :: PersistBackend m => T.Text -> [T.Text] -> m ()
insertFakeUsers = mapM_ . insertFakeUser

insertFakeUser :: PersistBackend m => T.Text -> T.Text -> m ()
insertFakeUser sid uid = do
    u <- getBy $ UserCoordKey (ServerId sid) (UserId uid)
    when (isNothing u) $ insert $ makeFakeUser sid uid

makeFakeUser :: T.Text -> T.Text -> User
makeFakeUser sid uid = User (ServerId sid) (UserId uid) (TopicId "about") (TopicId "invites")

runChatlessMigrate :: (ConnectionManager cm conn, MonadBaseControl IO m, MonadIO m, PersistBackend (DbPersist conn (NoLoggingT m))) => cm -> m ()
runChatlessMigrate = runDbConn $ runMigration defaultMigrationLogger $ do
    migrate (undefined :: User)
    migrate (undefined :: Topic)
    migrate (undefined :: Member)
    messageMigration
