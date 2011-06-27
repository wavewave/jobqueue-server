{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.Client.Command where

import HEP.Automation.Pipeline.Config 
import HEP.Automation.JobQueue.Client.Type 
import HEP.Automation.JobQueue.Client.Job

commandLineProcess :: JobClient -> IO () 
commandLineProcess (Get jid conf) = do 
  putStrLn "get called"
  readConfigFile conf >>= (flip startGetPhase) jid  
commandLineProcess (List qtyp conf) = do 
  putStrLn "list called"
  readConfigFile conf >>= (flip startListPhase) qtyp
commandLineProcess (Start conf) = do
  putStrLn "start called" 
  readConfigFile conf >>= startWaitPhase

--  jobqueueAssign (lc_clientConfiguration lc) 
--  getWebDAVInfo 

