{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.Client.Command where

import HEP.Automation.Pipeline.Config 
import HEP.Automation.JobQueue.Client.Type 
import HEP.Automation.JobQueue.Client.Phase

commandLineProcess :: JobClient -> IO () 
commandLineProcess (Get jid conf) = do 
  putStrLn "get called"
  readConfigFile conf >>= flip startGetPhase jid  
commandLineProcess (List qtyp conf) = do 
  putStrLn "list called"
  readConfigFile conf >>= flip startListPhase qtyp
commandLineProcess (Start conf) = do
  putStrLn "start called" 
  readConfigFile conf >>= flip startWaitPhase 3
commandLineProcess (StartTest conf) = do
  putStrLn "starttest called" 
  readConfigFile conf >>= flip startWaitTestPhase 3
commandLineProcess (Revert jid conf) = do 
  putStrLn $ "revert job " ++ show jid ++ " called"
  readConfigFile conf >>= flip startRevertPhase jid
commandLineProcess (Finish jid conf) = do 
  putStrLn $ "finish job " ++ show jid ++ " called"
  readConfigFile conf >>= flip startFinishPhase jid
commandLineProcess (Delete jid conf) = do 
  putStrLn $ "delete job " ++ show jid ++ " called"
  readConfigFile conf >>= flip startDeletePhase jid