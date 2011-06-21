{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.Client.Command where

import Network.HTTP.Types
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy as L 
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC

import HEP.Automation.MadGraph.Model 
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.SetupType


import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config

import HEP.Automation.JobQueue.Client.Type 

import Text.Parsec 

import Data.Aeson.Encode

testjob_psetup = PS MadGraph4 DummyModel "test" "test" "test" 
testjob_rsetup = RS DummyParam 10000 LHC7 Fixed 200.0 NoMatch NoCut NoPYTHIA NoUserCutDef NoPGS 1

testjob = EventSet testjob_psetup testjob_rsetup 

testjobdetail = EventGen testjob

jobqueueTest = do 
  putStrLn "test"
  manager <- newManager
  requestget <- parseUrl ("http://127.0.0.1:3600/")
  let testjson = toAeson testjob
 
  print testjson
  r <- httpLbs requestget manager
  putStrLn $ show r


jobqueueTest2 :: String -> IO () 
jobqueueTest2 str = do 
  putStrLn "test2"
  manager <- newManager 
  request <- parseUrl ("http://127.0.0.1:3600/queue")
  
  let testjson = encode $ toAeson testjobdetail 

  let myrequestbody = RequestBodyLBS testjson -- (C.pack testjson) 
  
  let requestpost = request { method = methodPost, 
                              requestHeaders = [ ("Content-Type", "text/plain") ], 
                              requestBody = myrequestbody } 
  r <- httpLbs requestpost manager 
  putStrLn $ show r 


jobqueueList :: IO () 
jobqueueList = do 
  putStrLn "list"
  manager <- newManager 
  requestget <- parseUrl ("http://127.0.0.1:3600/queuelist")
  
  let requestgetjson = requestget { 
        requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
      }

  r <- httpLbs requestgetjson manager 

  putStrLn $ show r 

jobqueueUnassigned :: IO () 
jobqueueUnassigned = do 
  putStrLn "unassigned"
  manager <- newManager 
  requestget <- parseUrl ("http://127.0.0.1:3600/queuelist/unassigned")
  
  let requestgetjson = requestget { 
        requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
      }
  r <- httpLbs requestgetjson manager 
  putStrLn $ show r 
 

commandLineProcess :: JobClient -> IO () 
commandLineProcess (JobClient act conf) = do 
  print act
  case act of
    "test" -> jobqueueTest 
    "test2" -> jobqueueTest2 "oohhhoooh"
    "list" -> jobqueueList
    "unassigned" -> jobqueueUnassigned


readConfigFile :: JobClient -> IO ClientConfiguration
readConfigFile (JobClient act conf) = do 
  putStrLn conf
  str <- readFile conf
  let r = parse clientConfigurationParse "" str
  case r of 
    Right result -> do putStrLn (show result) 
                       return result
    Left err -> error (show err)
                   

