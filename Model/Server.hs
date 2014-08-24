{-# LANGUAGE FlexibleInstances, QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs #-}
module Model.Server where
import Model.ID (ServerId)

data Server = Server { serverId :: ServerId } deriving (Show, Eq)
