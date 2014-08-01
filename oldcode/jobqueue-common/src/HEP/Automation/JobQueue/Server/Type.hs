
-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.JobQueue.Server.Type 
-- Copyright   : (c) 2011, 2012, 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module HEP.Automation.JobQueue.Server.Type where

import HEP.Storage.WebDAV.Type

data ServerConfig = ServerConfig { 
  server_main   :: String, 
  server_webdav :: URLtype -- WebDAVServer 
} deriving (Show)
