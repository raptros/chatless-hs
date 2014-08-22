{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.TopicMember where

import Data.Aeson
import Data.Aeson.TH
import Control.Lens hiding ((.=))

import Database.Groundhog.Core
import Database.Groundhog.TH

import Model.Utils
import Model.User
import Model.Topic


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

makeLensesWith (set lensField (\s -> Just (s ++ "Lens")) lensRules) ''MemberMode

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

data Member = Member {
    memberTopic :: TopicRef,
    memberUser :: UserRef,
    memberMode :: MemberMode 
}

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 6 } ''Member)

subsetAsJson :: UserRef -> MemberMode -> Value
subsetAsJson ur mm = object ["user" .= ur, "mode" .= mm]

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

instance NeverNull MemberMode

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

makeLensesWith (set lensField (\s -> Just (s ++ "Lens")) lensRules) ''MemberModeUpdate

resolveMemberModeUpdate :: MemberMode -> MemberModeUpdate -> MemberMode
resolveMemberModeUpdate mm mmu = mm &
    mmReadLens .~? mmu ^. mmuReadLens & 
    mmWriteLens .~? mmu ^. mmuWriteLens & 
    mmVoicedLens .~? mmu ^. mmuVoicedLens & 
    mmInviteLens .~? mmu ^. mmuInviteLens & 
    mmSetMemberLens .~? mmu ^. mmuSetMemberLens & 
    mmSetBannerLens .~? mmu ^. mmuSetBannerLens & 
    mmSetInfoLens .~? mmu ^. mmuSetInfoLens & 
    mmSetModeLens .~? mmu ^. mmuSetModeLens
