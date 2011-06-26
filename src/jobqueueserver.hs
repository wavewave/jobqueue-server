module Main where

import HEP.Automation.JobQueue.Server.Yesod
import HEP.Automation.JobQueue.JobQueue 

import Data.Acid 
import qualified Data.IntMap as M
import Yesod

main = do 
  putStrLn "jobqueueserver"
  acid <- openAcidState (JobInfoQueue 0 M.empty) 
  warpDebug 3600 (JobQueueServer acid)