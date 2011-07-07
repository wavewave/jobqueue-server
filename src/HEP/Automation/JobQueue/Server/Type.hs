module HEP.Automation.JobQueue.Server.Type where

import HEP.Storage.WebDAV.Type

data ServerConfig = ServerConfig { 
  server_main   :: String, 
  server_webdav :: WebDAVServer 
} deriving (Show)