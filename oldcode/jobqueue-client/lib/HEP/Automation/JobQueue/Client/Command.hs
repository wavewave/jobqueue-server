{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.Client.Command where

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.IO
 -- 
import qualified HEP.Automation.MadGraph.Log as MadGraphLog 
-- 
import HEP.Automation.JobQueue.Client.Job
import HEP.Automation.JobQueue.Client.Phase
import HEP.Automation.JobQueue.Client.Type 


commandLineProcess :: JobClient -> IO () 
commandLineProcess (Get jid url) = do 
    putStrLn "get called"
    startGetPhase (URL url) jid 
commandLineProcess (List qtyp url) = do 
    putStrLn "list called"
    startListPhase (URL url) qtyp 
commandLineProcess (Start conf) = do
  putStrLn "start called" 
  startLog MadGraphLog.defaultLogChan
  -- readConfigFile conf >>= \x -> startWaitPhase x 3 10 
commandLineProcess (StartTest conf) = do
  putStrLn "starttest called" 
  -- readConfigFile conf >>= flip startWaitTestPhase 3
commandLineProcess (Revert jid conf) = do 
  putStrLn $ "revert job " ++ show jid ++ " called"
  -- readConfigFile conf >>= flip startRevertPhase jid
commandLineProcess (Finish jid conf) = do 
  putStrLn $ "finish job " ++ show jid ++ " called"
  -- readConfigFile conf >>= flip startFinishPhase jid
commandLineProcess (Delete jid conf) = do 
  putStrLn $ "delete job " ++ show jid ++ " called"
  -- readConfigFile conf >>= flip startDeletePhase jid


startLog :: String -> IO () 
startLog logchanname = do 
  updateGlobalLogger logchanname (setLevel DEBUG)
  h <- streamHandler stderr DEBUG >>= \lh -> return $
         setFormatter lh 
           (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  updateGlobalLogger logchanname (addHandler h) 
