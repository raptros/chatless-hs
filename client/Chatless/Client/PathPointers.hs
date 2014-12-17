{-|
Description: reference parts of the api

types and tools to make it easy to reference parts of the chatless API
-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Chatless.Client.PathPointers where

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Tenc
import Web.PathPieces
import qualified Data.Sequence as S
import Data.Foldable (fold)
import Data.Monoid ((<>))

import Chatless.Model.ID

-- * working with paths
type Path = S.Seq T.Text

emptyPath :: Path
emptyPath = S.empty

type Sub = T.Text

-- | how to encode a pointer into a path
class PathPointer a where
    toPath :: a -> Path

instance PathPointer Path where
    toPath = id

pathSub :: PathPointer a => Sub -> a -> Path
pathSub sub p = toPath p S.|> sub

-- | puts all the slashes before all the elements of the path, and concats
-- the whole thing
mkAbsPath :: Path -> BS.ByteString
mkAbsPath pth = Tenc.encodeUtf8 $ fold $ pth >>= (S.singleton "/" S.|>)

-- * pointing at users

-- | pointer to a user
data UserPtr =
    MePtr |
    LocalUserPtr UserId |
    AnyUserPtr ServerId UserId
    deriving (Eq, Show)

instance PathPointer UserPtr where
    toPath MePtr = S.singleton "me"
    toPath (LocalUserPtr uid) = S.singleton "user" S.|> toPathPiece uid
    toPath (AnyUserPtr sid uid) = S.singleton "server" S.|> toPathPiece sid S.|> "user" S.|> toPathPiece uid

-- ** pointing at things users have

-- | name of topics sub
userTopicsSub :: Sub
userTopicsSub = "topic"

-- | name of about topics sub
userAboutSub :: Sub
userAboutSub = "about"

-- | name of invite topics sub
userInviteSub :: Sub
userInviteSub = "invite"

-- | path to the list of a user's topics
userTopicsPath :: UserPtr -> Path
userTopicsPath = pathSub userTopicsSub

-- ** constructing user pointers

-- * pointing at topics

-- | point to topics
data TopicPtr =
    AnyTopicPtr UserPtr TopicId |
    AboutTopicPtr UserPtr |
    InviteTopicPtr UserPtr
    deriving (Eq, Show)

instance PathPointer TopicPtr where
    toPath (AnyTopicPtr userPtr tid) = pathSub userTopicsSub userPtr S.|> toPathPiece tid
    toPath (AboutTopicPtr userPtr) = pathSub userAboutSub userPtr
    toPath (InviteTopicPtr userPtr) = pathSub userInviteSub userPtr

-- ** pointing at their members

-- | name of topic members sub
topicMembersSub :: Sub
topicMembersSub = "member"

-- | path to the base of a topic's members api
topicMembersPath :: TopicPtr -> Path
topicMembersPath = pathSub topicMembersSub

-- | pointer to a specific member in a topic
data MemberPtr =
    LocalMemberPtr TopicPtr UserId |
    AnyMemberPtr TopicPtr ServerId UserId
    deriving (Eq, Show)

instance PathPointer MemberPtr where
    toPath (LocalMemberPtr topicPtr uid) = pathSub topicMembersSub topicPtr S.|> "user" S.|> toPathPiece uid
    toPath (AnyMemberPtr topicPtr sid uid) = pathSub topicMembersSub topicPtr S.|> "server" S.|> toPathPiece sid S.|> "user" S.|> toPathPiece uid

-- ** referencing messages

-- | name of topic messages sub
topicMessagesSub :: Sub
topicMessagesSub = "message"

-- | path to the base of a topic's message api
topicMessagesPath :: TopicPtr -> Path
topicMessagesPath = pathSub topicMessagesSub

data MQBase = 
    MQFirst |
    MQLast |
    MQBefore MessageId |
    MQAfter MessageId |
    MQAt MessageId |
    MQFrom MessageId
    deriving (Eq, Show)

instance PathPointer MQBase where
    toPath MQFirst = S.singleton "first"
    toPath MQLast = S.singleton "last"
    toPath (MQBefore mid) = S.singleton "before" S.|> toPathPiece mid
    toPath (MQAfter mid) = S.singleton "after" S.|> toPathPiece mid
    toPath (MQAt mid) = S.singleton "at" S.|> toPathPiece mid
    toPath (MQFrom mid) = S.singleton "from" S.|> toPathPiece mid

-- | message query represents the way you can query a topic's messages
data MessageQueryPtr = MessageQueryPtr TopicPtr MQBase Int deriving (Eq, Show)

instance PathPointer MessageQueryPtr where
    toPath (MessageQueryPtr topicPtr mqbase count) = pathSub topicMessagesSub topicPtr <> toPath mqbase S.|> toPathPiece count

-- *** getting your hands on message queries 

queryFirst :: TopicPtr -> Int -> MessageQueryPtr
queryFirst = flip MessageQueryPtr MQFirst

queryLast :: TopicPtr -> Int -> MessageQueryPtr
queryLast = flip MessageQueryPtr MQLast

queryBefore :: TopicPtr -> MessageId -> Int -> MessageQueryPtr
queryBefore = (. MQBefore) . MessageQueryPtr

queryAfter :: TopicPtr -> MessageId -> Int -> MessageQueryPtr
queryAfter = (. MQAfter) . MessageQueryPtr

queryAt :: TopicPtr -> MessageId -> Int -> MessageQueryPtr
queryAt = (. MQAt) . MessageQueryPtr

queryFrom :: TopicPtr -> MessageId -> Int -> MessageQueryPtr
queryFrom = (. MQFrom) . MessageQueryPtr

-- *** referencing a specific message

data JustMessagePtr = JustMessagePtr TopicPtr MessageId deriving (Eq, Show)

instance PathPointer JustMessagePtr where
    toPath (JustMessagePtr topicPtr mid) = pathSub topicMessagesSub topicPtr S.|> "just" S.|> toPathPiece mid
