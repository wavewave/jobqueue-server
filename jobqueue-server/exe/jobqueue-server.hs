{-# LANGUAGE OverloadedStrings #-}

module Main where

import HEP.Automation.JobQueue.Server.Type
import HEP.Automation.JobQueue.Server.Yesod ()
import Yesod
import qualified Data.Map as M

main :: IO ()
main = do 
  putStrLn "jobqueue-server"
  warp 7800 (YesodcrudServer "server.db")
