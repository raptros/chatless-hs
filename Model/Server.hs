{-# LANGUAGE FlexibleInstances, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.Server where
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.TH
import Data.Aeson
import Data.Aeson.TH
import StorableJson
import Model.ID

data Server = Server { serverId :: ServerId } deriving (Show, Eq)
