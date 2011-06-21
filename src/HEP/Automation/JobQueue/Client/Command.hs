{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.Client.Command where

import HEP.Automation.JobQueue.Config
import HEP.Automation.JobQueue.Client.Type 
import HEP.Automation.JobQueue.Client.Job
import Text.Parsec 

commandLineProcess :: JobClient -> IO () 
commandLineProcess (List qtyp conf) = do 
  putStrLn "list called"
  case qtyp of 
    "all"        -> jobqueueList 
    "unassigned" -> jobqueueUnassigned
    "inprogress" -> jobqueueInprogress
    "finished"   -> jobqueueFinished
commandLineProcess (Assign jid conf) = do
  putStrLn "assign called" 
  putStrLn $ "jid = " ++ show jid
  cc <- readConfigFile conf
  jobqueueAssign cc jid 

readConfigFile :: FilePath -> IO ClientConfiguration
readConfigFile conf = do 
  putStrLn conf
  str <- readFile conf
  let r = parse clientConfigurationParse "" str
  case r of 
    Right result -> do putStrLn (show result) 
                       return result
    Left err -> error (show err) 

