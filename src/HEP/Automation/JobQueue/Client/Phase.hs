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
          -- * Revert command 
        , startRevertPhase
        ) where

import Control.Concurrent (threadDelay)

import HEP.Automation.Pipeline.Config
import HEP.Automation.Pipeline.Job
import HEP.Automation.Pipeline.Job.Match

import HEP.Automation.JobQueue.JobQueue
-- import HEP.Automation.JobQueue.Server.Type
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
      r' <- getWebDAVInfo url
      case r' of 
        Nothing -> startWaitPhase lc
        Just sconf -> do 
          let wc = WorkConfig lc sconf
              job = jobMatch jinfo
              back = backToUnassigned url jinfo >> startWaitPhase lc
          putStrLn $ "Work Configuration = " ++ show wc
          b1 <- pipeline_checkSystem job wc jinfo'
          if not b1 
            then back
            else do 
              changeStatus url jinfo' BeingCalculated
              b2 <- pipeline_startWork job wc jinfo' 
              if not b2 
                then back
                else do
                  changeStatus url jinfo' BeingTested
                  b3 <- pipeline_startTest job wc jinfo'
                  if not b3 
                    then back
                    else do 
                      b4 <- pipeline_uploadWork job wc jinfo'
                      if not b4 
                        then back
                        else do
                          changeStatus url jinfo' Finished
                          threadDelay 10000000
                          return ()
  startWaitPhase lc

startGetPhase :: LocalConfiguration -> Int -> IO () 
startGetPhase lc jid = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  jobqueueGet url jid
  return ()   

startListPhase :: LocalConfiguration -> String -> IO () 
startListPhase lc qtyp = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  case qtyp of 
    "all"        -> jobqueueList url
    "unassigned" -> jobqueueUnassigned url
    "inprogress" -> jobqueueInprogress url
    "finished"   -> jobqueueFinished url
    _ -> putStrLn "No such option"

startRevertPhase :: LocalConfiguration -> Int -> IO () 
startRevertPhase lc jid = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  j <- jobqueueGet url jid
  case j of 
    Left errmsg -> do
      putStrLn "No such job!"
      putStrLn $ "error : " ++ errmsg
    Right jobinfo -> do 
      putStrLn $ " job is " ++ show jobinfo
      backToUnassigned url jobinfo
      return ()
