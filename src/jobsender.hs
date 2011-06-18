module Main where

import System.Environment
import HEP.Automation.JobQueue.Client.Command

main = do 
  args <- getArgs
  putStrLn "jobsender"
  commandLineProcess args 
