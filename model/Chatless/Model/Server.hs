module Chatless.Model.Server where
import Chatless.Model.ID (ServerId)

data Server = Server { serverId :: ServerId } deriving (Show, Eq)
