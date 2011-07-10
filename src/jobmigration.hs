module Main where

import HEP.Automation.JobQueue.JobQueue -- .V3 as HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.JobType -- .V3 as HEP.Automation.JobQueue.JobType 

import Data.Acid
import qualified Data.IntMap as M

main :: IO ()
main = do 
  putStrLn "job migration."
  acid <- openAcidStateFrom "state/HEP.Automation.JobQueue.JobQueue.JobInfoQueue" (JobInfoQueue 0 M.empty) 
  r <- query acid QueryAll 
  putStrLn $ show r 
