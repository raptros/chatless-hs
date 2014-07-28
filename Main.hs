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

import Model.User
import Model.Topic
import Model.TopicMember
import Model.ID
import Model.StorableJson
import Api
import Api.Root

main :: IO ()
main = do
    pool <- createSqlitePool ":memory:" 1
    runChatlessMigrate pool
    (flip runDbConn) pool $ do
        insert $ makeFakeUser "user0"
        insert $ makeFakeUser "user1"
    warp 3000 $ Chatless {
        backendConn = pool,
        localServer = ServerId "local"
    }

makeFakeUser :: T.Text -> User
makeFakeUser id = User (ServerId "local") (UserId id) (TopicId "about") (TopicId "invites")

runChatlessMigrate :: (ConnectionManager cm conn, MonadBaseControl IO m, MonadIO m, PersistBackend (DbPersist conn (NoLoggingT m))) => cm -> m ()
runChatlessMigrate = runDbConn $ runMigration defaultMigrationLogger $ do
    migrate (undefined :: User)
    migrate (undefined :: Topic)
    migrate (undefined :: Member)
