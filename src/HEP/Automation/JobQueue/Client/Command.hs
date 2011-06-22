{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.Client.Command where

import HEP.Automation.JobQueue.Config
import HEP.Automation.Pipeline.Config 
import HEP.Automation.JobQueue.Client.Type 
import HEP.Automation.JobQueue.Client.Job
import Text.Parsec 

commandLineProcess :: JobClient -> IO () 
commandLineProcess (Get jid conf) = do 
  putStrLn "get called"
  putStrLn (show jid)
  jobqueueGet jid 
commandLineProcess (List qtyp conf) = do 
  putStrLn "list called"
  case qtyp of 
    "all"        -> jobqueueList 
    "unassigned" -> jobqueueUnassigned
    "inprogress" -> jobqueueInprogress
    "finished"   -> jobqueueFinished
commandLineProcess (Assign conf) = do
  putStrLn "assign called" 
  lc <- readConfigFile conf
  jobqueueAssign (lc_clientConfiguration lc) 


