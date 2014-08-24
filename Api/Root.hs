{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Api.Root where

import Data.Pool (Pool)
import qualified Data.Text as T
import Data.Aeson ((.=))

import Database.Groundhog.Sqlite (Sqlite)
--import Database.Groundhog.Postgresql
import Database.Groundhog.Core (ConnectionManager(..))

import Yesod.Core (Yesod(..), RenderRoute(..), ErrorResponse(..))
import Yesod.Core.Dispatch (mkYesodData, parseRoutes)

import Model.ID (ServerId, UserId, TopicId)

import Api.Utils (returnErrorObject)
import Api.TopicSub.Data (TopicSub(..))


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
      where corrected1 = filterLast (not . T.null) s
            corrected2 = filter (not . T.null) s
            dropDash t
                | T.all (== '-') t = T.drop 1 t
                | otherwise = t

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast p (a:[]) = if (p a) then [a] else []
filterLast p (a:as) = a:(filterLast p as)


mkYesodData "Chatless" [parseRoutes|
/me MeR GET
/me/about MeAboutTopicSubR TopicSub getMeAboutTopicSub 
/me/invite MeInviteTopicSubR TopicSub getMeInviteTopicSub
/me/topic MeTopicsR GET POST
/me/topic/#TopicId MeTopicSubR TopicSub getMeTopicSub
-- subscription system
/me/sub/user/#UserId/topic/#TopicId SubsLocalR PUT
-- local users
/user/#UserId LocalUserR GET
/user/#UserId/about LocalUserAboutTopicSubR TopicSub getLocalUserAboutTopicSub
/user/#UserId/invite LocalUserInviteTopicSubR TopicSub getLocalUserInviteTopicSub
/user/#UserId/topic LocalUserTopicsR GET
/user/#UserId/topic/#TopicId LocalUserTopicSubR TopicSub getLocalUserTopicSub
-- todo remote topics
-- /server/#ServerId/user/#UserId AnyUserR GET
-- /server/#ServerId/user/#UserId/topic AnyUserTopicsR GET
-- /server/#ServerId/user/#UserId/ AnyUserR GET
-- /server/#ServerId/user/#UserId/about AnyUserAboutTopicSubR TopicSub getAnyUserAboutTopicSub
-- /server/#ServerId/user/#UserId/invite AnyUserInviteTopicSubR TopicSub getAnyUserInviteTopicSub
-- /server/#ServerId/user/#UserId/topic/#TopicId AnyUserTopicSubR TopicSub getAnyUserTopicSub
|]

giveServer :: (ServerId -> b) -> Chatless -> b
giveServer = (. localServer)

getMeAboutTopicSub = const MeAboutTopicSub
getMeInviteTopicSub = const MeInviteTopicSub
getMeTopicSub = const MeTopicSub

getLocalUserAboutTopicSub = const LocalUserAboutTopicSub 
getLocalUserInviteTopicSub = const LocalUserInviteTopicSub 
getLocalUserTopicSub = const LocalUserTopicSub

getAnyUserAboutTopicSub = const AnyUserAboutTopicSub
getAnyUserInviteTopicSub = const AnyUserInviteTopicSub
getAnyUserTopicSub = const AnyUserTopicSub

