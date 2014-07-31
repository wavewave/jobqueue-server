module Main where

import System.Console.CmdArgs
--
import HEP.Automation.JobQueue.Client.ProgType
import HEP.Automation.JobQueue.Client.Command

main :: IO () 
main = do 
  putStrLn "jobqueue-client"
  param <- cmdArgs mode
  commandLineProcess param
