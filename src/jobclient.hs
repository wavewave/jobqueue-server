{-# LANGUAGE DeriveDataTypeable #-}

module Main where

-- import System.Environment
import System.Console.CmdArgs
import HEP.Automation.JobQueue.Client.Command
import HEP.Automation.JobQueue.Client.Type

main = do 
-- args <- getArgs
  putStrLn "jobsender"
  param <- cmdArgs defParam

  readConfigFile param 
--  commandLineProcess param 
