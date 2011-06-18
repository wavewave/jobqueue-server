module Main where

import HEP.Automation.JobQueue.JobQueueYesod
import Yesod

main = do 
  putStrLn "jobqueueserver"
  warpDebug 3600 JobQueueServer