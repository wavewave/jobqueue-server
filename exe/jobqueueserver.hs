module Main where

import HEP.Automation.JobQueue.Server.Yesod
import HEP.Automation.JobQueue.Server.Work
import HEP.Automation.JobQueue.JobQueue 

import Data.Acid 
import qualified Data.IntMap as M
import Yesod

main :: IO ()
main = do 
  putStrLn "jobqueueserver"
  acid <- openLocalState (JobInfoQueue 0 M.empty) 
  sconf <- serverConfigParser "test.conf"
  warpDebug 3600 (JobQueueServer acid sconf) 

  createCheckpoint acid 
