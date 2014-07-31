{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import HEP.Automation.JobQueue.Client.Command
import HEP.Automation.JobQueue.Client.Type

main :: IO ()
main = do 
  putStrLn "jobclient"
  param <- cmdArgs mode 

  putStrLn $ show param 
  commandLineProcess param 
