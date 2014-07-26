{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Api.Root where

import Api.Utils
import Yesod.Core
import Api.TopicSub.Data

import Model.ID
import Model.User
import Model.Topic
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Sqlite
import Database.Groundhog.Postgresql
import Database.Groundhog.Core (ConnectionManager(..))
import Control.Monad (mfilter)
import Control.Monad.Reader
import Data.Pool
import Safe

type CLBackend = Sqlite

data Chatless = Chatless {
    backendConn :: Pool CLBackend,
    localServer :: ServerId
}

instance ConnectionManager Chatless CLBackend where
    withConn f app = withConn f (backendConn app)
    withConnNoTransaction f app = withConnNoTransaction f (backendConn app)

instance Yesod Chatless where
    errorHandler NotFound               = returnErrorObject "not_found"         []
    errorHandler NotAuthenticated       = returnErrorObject "not_authenticated" []
    errorHandler (InternalError msg)    = returnErrorObject "internal_error"    ["error"  .= msg]
    errorHandler (InvalidArgs errs)     = returnErrorObject "invalid_args"      ["errors" .= errs]
    errorHandler (BadMethod method)     = returnErrorObject "bad_method"        ["method" .= show method]
    errorHandler (PermissionDenied why) = returnErrorObject "permission_denied" ["why"    .= why]

    cleanPath _ s = if corrected1 == corrected2 then Right $ map dropDash corrected1 else Left corrected2
      where corrected1 = filterTail (not . T.null) s
            corrected2 = filter (not . T.null) s
            dropDash t
                | T.all (== '-') t = T.drop 1 t
                | otherwise = t

filterTail :: (a -> Bool) -> [a] -> [a]
filterTail _ [] = []
filterTail p (a:[]) = if (p a) then [a] else []
filterTail p (a:as) = a:(filterTail p as)


mkYesodData "Chatless" [parseRoutes|
/me MeR GET
/me/about MeAboutTopicSubR TopicSub getMeAboutTopicSub 
/me/invites MeInvitesTopicSubR TopicSub getMeInvitesTopicSub
/me/topic MeTopicsR GET POST
/me/topic/#TopicId MeTopicSubR TopicSub getMeTopicSub
/user/#UserId LocalUserR GET
/user/#UserId/about LocalUserAboutTopicSubR TopicSub getLocalUserAboutTopicSub
/user/#UserId/invites LocalUserInvitesTopicSubR TopicSub getLocalUserInvitesTopicSub
/user/#UserId/topic LocalUserTopicsR GET
/user/#UserId/topic/#TopicId LocalUserTopicSubR TopicSub getLocalUserTopicSub
-- /server/#ServerId/user/#UserId AnyUserR GET
-- /server/#ServerId/user/#UserId/topic AnyUserTopicsR GET
-- /server/#ServerId/user/#UserId/ AnyUserR GET
|]

giveServer :: (ServerId -> b) -> Chatless -> b
giveServer = (. localServer)

getMeAboutTopicSub = giveServer MeAboutTopicSub
getMeInvitesTopicSub = giveServer MeInvitesTopicSub
getMeTopicSub = giveServer MeTopicSub

getLocalUserAboutTopicSub = giveServer LocalUserAboutTopicSub 
getLocalUserInvitesTopicSub = giveServer LocalUserInvitesTopicSub 
getLocalUserTopicSub = giveServer LocalUserTopicSub
