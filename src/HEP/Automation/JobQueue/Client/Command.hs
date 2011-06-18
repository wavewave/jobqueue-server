{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.Client.Command where

import Network.HTTP.Types
import Network.HTTP.Enumerator


jobqueueTest = do 
  putStrLn "test"
  manager <- newManager
  requestget <- parseUrl ("http://127.0.0.1:3600/")
  r <- httpLbs requestget manager
  putStrLn $ show r



commandLineProcess :: [String] -> IO () 
commandLineProcess args = do 
  print args 
  case args !! 0 of
    "test" -> jobqueueTest 

