{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, TypeSynonymInstances #-}
module Api.TopicSub.Data where

import Yesod.Core (RenderRoute(..))
import Yesod.Core.Dispatch (mkYesodSubData, parseRoutes)

import Model.ID (ServerId, UserId, TopicId, MessageId)

data TopicSub = MeTopicSub TopicId |
                MeAboutTopicSub |
                MeInviteTopicSub |
                LocalUserTopicSub UserId TopicId |
                LocalUserAboutTopicSub UserId |
                LocalUserInviteTopicSub UserId |
                AnyUserAboutTopicSub ServerId UserId |
                AnyUserInviteTopicSub ServerId UserId |
                AnyUserTopicSub ServerId UserId TopicId


mkYesodSubData "TopicSub" [parseRoutes|
/ TopicR GET
/mode TopicModeR GET PUT
/member MembersR GET
/member/user/#UserId LocalMemberR GET PUT POST
/member/server/#ServerId/user/#UserId MemberR GET PUT POST
/message MsgR GET POST
/message/first MsgFirst1R GET
-- todo limit the number of messages
/message/first/#Int MsgFirstR GET
/message/last MsgLast1R GET
/message/last/#Int MsgLastR GET
/message/before/#MessageId MsgBefore1R GET
/message/before/#MessageId/#Int MsgBeforeR GET
/message/after/#MessageId MsgAfter1R GET
/message/after/#MessageId/#Int MsgAfterR GET
/message/at/#MessageId MsgAt1R GET
/message/at/#MessageId/#Int MsgAtR GET
/message/from/#MessageId MsgFrom1R GET
/message/from/#MessageId/#Int MsgFromR GET
|]
