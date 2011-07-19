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
          -- * Finish command
        , startFinishPhase
          -- * Delete command
        , startDeletePhase
        ) where

import Control.Concurrent (threadDelay)

import HEP.Automation.Pipeline.Config
import HEP.Automation.Pipeline.Job
import HEP.Automation.Pipeline.Job.Match

import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config 
-- import HEP.Automation.JobQueue.Server.Type
import HEP.Automation.JobQueue.Client.Job

startWaitPhase :: LocalConfiguration -> Int -> IO () 
startWaitPhase lc n = do 
  putStrLn "starting Wait Phase"
  newn <- if (n < 0) 
            then do 
              putStrLn "too many failure, need to take a rest" 
              threadDelay (60*1000000)
              return 3 
            else return n
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  r <- jobqueueAssign url (lc_clientConfiguration lc) 
  case r of 
    Just jinfo -> startJobPhase lc jinfo newn
    Nothing -> do
      threadDelay . (*1000000) . nc_polling . lc_networkConfiguration $ lc
      startWaitPhase lc newn

startJobPhase :: LocalConfiguration -> JobInfo -> Int -> IO ()
startJobPhase lc jinfo n = do 
  putStrLn "starting Job Phase"
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  let cname = computerName . lc_clientConfiguration $ lc
  -- check job here
  r <- confirmAssignment url cname jinfo 
  case r of
    Nothing -> startWaitPhase lc (n-1)
    Just jinfo' -> do 
      putStrLn "job assigned well"
      r' <- getWebDAVInfo url
      case r' of 
        Nothing -> startWaitPhase lc n
        Just sconf -> do 
          let wc = WorkConfig lc sconf
              job = jobMatch jinfo
              back = backToUnassigned url jinfo >> startWaitPhase lc (n-1)
          putStrLn $ "Work Configuration = " ++ show wc
          b1 <- pipeline_checkSystem job wc jinfo'
          threadDelay 10000000
          if not b1 
            then back
            else do 
              changeStatus url jinfo' (BeingCalculated cname)
              b2 <- pipeline_startWork job wc jinfo' 
              threadDelay 10000000
              if not b2 
                then back
                else do
                  changeStatus url jinfo' (BeingTested cname)
                  b3 <- pipeline_startTest job wc jinfo'
                  threadDelay 10000000
                  if not b3 
                    then back
                    else do 
                      b4 <- pipeline_uploadWork job wc jinfo'
                      threadDelay 10000000
                      if not b4 
                        then back
                        else do
                          changeStatus url jinfo' (Finished cname)
                          threadDelay 10000000
                          return ()
  startWaitPhase lc n 

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

startFinishPhase :: LocalConfiguration -> Int -> IO () 
startFinishPhase lc jid = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  j <- jobqueueGet url jid
  case j of 
    Left errmsg -> do
      putStrLn "No such job!"
      putStrLn $ "error : " ++ errmsg
    Right jobinfo -> do 
      putStrLn $ " job is " ++ show jobinfo
      makeFinished url jobinfo
      return ()


startDeletePhase :: LocalConfiguration -> Int -> IO () 
startDeletePhase lc jid = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  j <- jobqueueGet url jid
  case j of 
    Left errmsg -> do
      putStrLn "No such job!"
      putStrLn $ "error : " ++ errmsg
    Right jobinfo -> do 
      putStrLn $ " job is " ++ show jobinfo
      jobqueueDelete url jid
      return ()
