{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Chatless.Model.TopicMember where

import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Control.Lens.TH (makeLensesWith, lensRules, lensField)
import Control.Lens.Operators ((&), (.~))

import Database.Groundhog.TH (mkPersist, defaultCodegenConfig, groundhog)

import Chatless.Model.Utils (dropAndLowerHead, runUpdateR, (^.=?), asMaybe, mkLensName)
import Chatless.Model.User (UserRef)
import Chatless.Model.Topic (TopicMode(..), TopicRef)


data MemberMode = MemberMode {
    mmRead :: Bool,
    mmWrite :: Bool,
    mmVoiced :: Bool,
    mmInvite :: Bool,
    mmSetMember :: Bool,
    mmSetBanner :: Bool,
    mmSetInfo :: Bool,
    mmSetMode :: Bool
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 2 } ''MemberMode)

makeLensesWith (lensRules & lensField .~ mkLensName) ''MemberMode

modeCreator :: MemberMode
modeCreator = MemberMode {
    mmRead = True,
    mmWrite = True,
    mmVoiced = True,
    mmInvite = True,
    mmSetMember = True,
    mmSetBanner = True,
    mmSetInfo = True,
    mmSetMode = True
}

modeDeny :: MemberMode
modeDeny = MemberMode {
    mmRead = False,
    mmWrite = False,
    mmVoiced = False,
    mmInvite = False,
    mmSetMember = False,
    mmSetBanner = False,
    mmSetInfo = False,
    mmSetMode = False
}

nonMemberMode :: TopicMode -> MemberMode
nonMemberMode tm = modeDeny { mmRead = readable tm && not (membersOnly tm) }

joinerMode :: TopicMode -> MemberMode 
joinerMode tm = modeDeny { mmRead = readable tm, mmWrite = writable tm }

invitedMode :: TopicMode -> MemberMode
invitedMode tm = modeDeny { mmRead = True, mmWrite = writable tm }

canSend :: TopicMode -> MemberMode -> Bool
canSend tm mm = mmWrite mm && (not (muted tm) || mmVoiced mm)

canSetBanner :: TopicMode -> MemberMode -> Bool
canSetBanner = const mmSetBanner

data Member = Member {
    memberTopic :: TopicRef,
    memberUser :: UserRef,
    memberMode :: MemberMode 
}

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 6 } ''Member)

data MemberPartial = MemberPartial {
    mPartUser :: UserRef,
    mPartMode :: MemberMode
} deriving (Eq, Show)

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 5 } ''MemberPartial)

mkPersist defaultCodegenConfig [groundhog|
- embedded: MemberMode
  fields:
    - name: mmRead
      dbName: read
    - name: mmWrite
      dbName: write
    - name: mmVoiced
      dbName: voiced
    - name: mmInvite
      dbName: invite
    - name: mmSetMember
      dbName: setMember
    - name: mmSetBanner
      dbName: setBanner
    - name: mmSetInfo
      dbName: setInfo
    - name: mmSetMode
      dbName: setMode
- entity: Member
  autoKey: null
  keys:
    - name: TargetMember
      default: true
  constructors:
    - name: Member
      uniques:
        - name: TargetMember
          type: primary
          fields: [memberTopic, memberUser]
      fields:
        - name: memberTopic
          dbName: topic
        - name: memberUser
          dbName: user
        - name: memberMode
          dbName: mode
|]

data MemberModeUpdate = MemberModeUpdate {
    mmuRead :: Maybe Bool,
    mmuWrite :: Maybe Bool,
    mmuVoiced :: Maybe Bool,
    mmuInvite :: Maybe Bool,
    mmuSetMember :: Maybe Bool,
    mmuSetBanner :: Maybe Bool,
    mmuSetInfo :: Maybe Bool,
    mmuSetMode :: Maybe Bool
} deriving (Eq, Show)

defaultMMU :: MemberModeUpdate
defaultMMU = MemberModeUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 3 } ''MemberModeUpdate)

makeLensesWith (lensRules & lensField .~ mkLensName) ''MemberModeUpdate

resolveMemberModeUpdate :: MemberMode -> MemberModeUpdate -> (Bool, MemberMode)
resolveMemberModeUpdate = runUpdateR $ do
    mmReadLens ^.=? mmuReadLens
    mmWriteLens ^.=? mmuWriteLens
    mmVoicedLens ^.=? mmuVoicedLens
    mmInviteLens ^.=? mmuInviteLens
    mmSetMemberLens ^.=? mmuSetMemberLens
    mmSetBannerLens ^.=? mmuSetBannerLens
    mmSetInfoLens ^.=? mmuSetInfoLens
    mmSetModeLens ^.=? mmuSetModeLens

resolveMemberModeUpdateMay :: MemberMode -> MemberModeUpdate -> Maybe MemberMode
resolveMemberModeUpdateMay = (asMaybe .) . resolveMemberModeUpdate
