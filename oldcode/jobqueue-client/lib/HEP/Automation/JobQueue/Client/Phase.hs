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
{-         ( -- * Get command 
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
          -- * For testing 
        , startWaitTestPhase
        , startJobTestPhase
        )  -}
where

import Control.Concurrent (threadDelay)

-- import HEP.Automation.Pipeline.Config
-- import HEP.Automation.Pipeline.Job
-- import HEP.Automation.Pipeline.Job.Match
-- import HEP.Automation.Pipeline.Job.DummyTest

import HEP.Automation.EventGeneration.Config
import HEP.Automation.EventGeneration.Deploy
import HEP.Automation.EventGeneration.Job
import HEP.Automation.EventGeneration.Type
import HEP.Automation.EventGeneration.Util
import HEP.Automation.EventGeneration.Work
--
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config 
import HEP.Automation.JobQueue.Client.Job

import Data.Aeson.Types 
import Control.Monad


newtype URL = URL {unURL :: String}

-- |
startListPhase :: URL -> String -> IO () 
startListPhase (URL url) qtyp = do
  -- let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  case qtyp of 
    "all"        -> jobqueueList url
    "unassigned" -> jobqueueUnassigned url
    "inprogress" -> jobqueueInprogress url
    "finished"   -> jobqueueFinished url
    _ -> putStrLn "No such option"


startGetPhase :: URL -> Int -> IO () 
startGetPhase (URL url) jid = do
  jobqueueGet url jid >>= print
  return ()   

{- 
startWaitPhase :: LocalConfiguration -> Int -> Int -> IO () 
startWaitPhase lc n assignfailure = do 
  putStrLn "starting Wait Phase"
  when (assignfailure < 0) $ do 
      putStrLn "too many assign failure. kill the process" 
      error "assign failure kill"
  newn <- if (n < 0) 
            then do 
              putStrLn "too many failure, need to take a rest" 
              threadDelay (60*1000000)
              return 3 
            else return n
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  r <- jobqueueAssign url (lc_clientConfiguration lc) 
  case r of 
    Just jinfo -> startJobPhase lc jinfo newn 10
    Nothing -> do
      putStrLn "assign failure" 
      putStrLn ("remaining chance : " ++ show assignfailure)
      threadDelay . (*1000000) . nc_polling . lc_networkConfiguration $ lc
      startWaitPhase lc newn (assignfailure-1)

startJobPhase :: LocalConfiguration -> JobInfo -> Int -> Int -> IO ()
startJobPhase lc jinfo n af = do 
  putStrLn "starting Job Phase"
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  let cname = computerName . lc_clientConfiguration $ lc
  -- check job here
  r <- confirmAssignment url cname jinfo 
  case r of
    Nothing -> startWaitPhase lc (n-1) af
    Just jinfo' -> do 
      putStrLn "job assigned well"
      r' <- getWebDAVInfo url
      case r' of 
        Error err -> startWaitPhase lc n af
        Success sconf -> do 
          let wc = WorkConfig lc sconf
              job = jobMatch jinfo
              back = backToUnassigned url jinfo >> startWaitPhase lc (n-1) af
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
  startWaitPhase lc n af

-- |


-- |


-- | 

startRevertPhase :: LocalConfiguration -> Int -> IO () 
startRevertPhase lc jid = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  j <- jobqueueGet url jid
  case j of 
    Error errmsg -> do
      putStrLn "No such job!"
      putStrLn $ "error : " ++ errmsg
    Success jobinfo -> do 
      putStrLn $ " job is " ++ show jobinfo
      backToUnassigned url jobinfo
      return ()

-- |

startFinishPhase :: LocalConfiguration -> Int -> IO () 
startFinishPhase lc jid = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  j <- jobqueueGet url jid
  case j of 
    Error errmsg -> do
      putStrLn "No such job!"
      putStrLn $ "error : " ++ errmsg
    Success jobinfo -> do 
      putStrLn $ " job is " ++ show jobinfo
      makeFinished url jobinfo
      return ()

-- | 

startDeletePhase :: LocalConfiguration -> Int -> IO () 
startDeletePhase lc jid = do
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  j <- jobqueueGet url jid
  case j of 
    Error errmsg -> do
      putStrLn "No such job!"
      putStrLn $ "error : " ++ errmsg
    Success jobinfo -> do 
      putStrLn $ " job is " ++ show jobinfo
      jobqueueDelete url jid
      return ()


----------

startWaitTestPhase :: LocalConfiguration -> Int -> IO () 
startWaitTestPhase lc n = do 
  putStrLn "starting WaitTest Phase"
  newn <- if (n < 0) 
            then do 
              putStrLn "too many failure, need to take a rest" 
              threadDelay (60*1000000)
              return 3 
            else return n
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  r <- jobqueueAssign url (lc_clientConfiguration lc) 
  case r of 
    Just jinfo -> startJobTestPhase lc jinfo newn
    Nothing -> do
      threadDelay . (*1000000) . nc_polling . lc_networkConfiguration $ lc
      startWaitTestPhase lc newn

startJobTestPhase :: LocalConfiguration -> JobInfo -> Int -> IO ()
startJobTestPhase lc jinfo n = do 
  putStrLn "starting Job Phase"
  let url = nc_jobqueueurl . lc_networkConfiguration $ lc
  let cname = computerName . lc_clientConfiguration $ lc
  -- check job here
  r <- confirmAssignment url cname jinfo 
  case r of
    Nothing -> startWaitTestPhase lc (n-1)
    Just jinfo' -> do 
      putStrLn "job assigned well"
      r' <- getWebDAVInfo url
      case r' of 
        Error str -> startWaitTestPhase lc n
        Success sconf -> do 
          let wc = WorkConfig lc sconf
              -- job = jobMatch jinfo
              testjob = dummyTestJob
              back = backToUnassigned url jinfo >> startWaitTestPhase lc (n-1)
          putStrLn $ "Work Configuration = " ++ show wc
          b1 <- pipeline_checkSystem testjob wc jinfo'
          threadDelay 10000000
          if not b1 
            then back
            else do 
              changeStatus url jinfo' (BeingCalculated cname)
              b2 <- pipeline_startWork testjob wc jinfo' 
              threadDelay 10000000
              if not b2 
                then back
                else do
                  changeStatus url jinfo' (BeingTested cname)
                  b3 <- pipeline_startTest testjob wc jinfo'
                  threadDelay 10000000
                  if not b3 
                    then back
                    else do 
                      b4 <- pipeline_uploadWork testjob wc jinfo'
                      threadDelay 10000000
                      if not b4 
                        then back
                        else do
                          changeStatus url jinfo' (Finished cname)
                          threadDelay 10000000
                          return ()
  startWaitTestPhase lc n 
-}