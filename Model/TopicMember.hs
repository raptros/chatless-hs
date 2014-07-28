{-# LANGUAGE TypeFamilies, FlexibleInstances, QuasiQuotes, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.TopicMember where

import Data.Text hiding (drop)
import qualified Data.Char as C

import Data.Aeson
import Data.Aeson.TH

import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.TH

import Model.Utils
import Model.ID
import Model.StorableJson
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
}

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 2 } ''MemberMode)

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

data Member = Member {
    memberTopic :: TopicRef,
    memberUser :: UserRef,
    memberMode :: MemberMode 
}

$(deriveJSON defaultOptions { fieldLabelModifier = dropAndLowerHead 6 } ''Member)

subsetAsJson :: UserRef -> MemberMode -> Value
subsetAsJson ur mm = object ["user" .= ur, "mode" .= mm]

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
