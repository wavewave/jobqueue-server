module HEP.Automation.JobQueue.Client.Phase where

import Control.Concurrent (threadDelay)

import HEP.Automation.Pipeline.Config
import HEP.Automation.JobQueue.JobQueue

import HEP.Automation.JobQueue.Client.Job

startWaitPhase :: LocalConfiguration -> IO () 
startWaitPhase lc = do 
  putStrLn "starting Wait Phase"
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  r <- jobqueueAssign url (lc_clientConfiguration lc) 
  case r of 
    Just _jinfo -> getWebDAVInfo url 
    Nothing -> do
      threadDelay . (*1000000) . nc_polling . lc_networkConfiguration $ lc
      startWaitPhase lc

startJobPhase :: LocalConfiguration -> JobInfo -> IO ()
startJobPhase lc jinfo = do 
  putStrLn "starting Job Phase"
  putStrLn $ show lc
  putStrLn $ show jinfo

startGetPhase :: LocalConfiguration -> Int -> IO () 
startGetPhase lc jid = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  jobqueueGet url jid
  

startListPhase :: LocalConfiguration -> String -> IO () 
startListPhase lc qtyp = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  case qtyp of 
    "all"        -> jobqueueList url
    "unassigned" -> jobqueueUnassigned url
    "inprogress" -> jobqueueInprogress url
    "finished"   -> jobqueueFinished url
    _ -> putStrLn "No such option"
