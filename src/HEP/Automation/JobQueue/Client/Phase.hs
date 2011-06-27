{- |
Module       : HEP.Automation.JobQueue.Client.Phase
Copyright    : Ian-Woo Kim
License      : BSD3

Maintainer   : Ian-Woo Kim <ianwookim@gmail.com>
Stability    : Experimental
Portability  : unknown 
 
jobqueue-client driver routine and eventloop for each phase

This module is a collection of startXXXXPhase routines, which
are a driver procedure for each phase. 

-}

module HEP.Automation.JobQueue.Client.Phase 
        ( -- * Get command 
          startGetPhase
          -- * List command
        , startListPhase
          -- * Start command
        , startWaitPhase 
        , startJobPhase
        ) where

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
    Just jinfo -> startJobPhase lc jinfo
    Nothing -> do
      threadDelay . (*1000000) . nc_polling . lc_networkConfiguration $ lc
      startWaitPhase lc

startJobPhase :: LocalConfiguration -> JobInfo -> IO ()
startJobPhase lc jinfo = do 
  putStrLn "starting Job Phase"
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  
  -- check job here
  r <- confirmAssignment url jinfo 
  case r of
    Nothing -> startWaitPhase lc
    Just jinfo' -> do 
      putStrLn "job assigned well"
      getWebDAVInfo url 

  -- main job will be here
 
  -- return to Wait Phase
  startWaitPhase lc

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
