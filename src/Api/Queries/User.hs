{-|

user queries
-}
module Api.Queries.User where

import Network.Wai (ResponseReceived)

import qualified Database.Groundhog as Gh

import Web.Respond

import qualified Chatless.Model.Topic as Tp
import qualified Chatless.Model.User as Ur
import Api.Queries.Base
import Api.Monad

-- | run an action on a user ... if that user can be found in the db
withUser :: (MonadChatless m, MonadRespond m) => (Ur.User -> m ResponseReceived) -> Ur.UserRef -> m ResponseReceived
withUser act uref = runQuery (Gh.getBy uref) >>= maybe notFound act
    where notFound = respondNotFound (UserNotFound uref)

-- | list out the topics a user has created
getUserTopics :: MonadChatless m => Ur.User -> m [Tp.TopicRef]
getUserTopics = runQuery . userTopicsQuery

-- | the groundhog query.
userTopicsQuery :: Gh.PersistBackend m => Ur.User -> m [Tp.TopicRef]
userTopicsQuery user = Gh.project Tp.TopicCoord $ (Tp.TopicServerField Gh.==. Ur.userServer user) Gh.&&. (Tp.TopicUserField Gh.==. Ur.userId user)

