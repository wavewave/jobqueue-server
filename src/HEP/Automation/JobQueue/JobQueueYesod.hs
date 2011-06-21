{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

----------------------------------------------------
--
-- Module       : HEP.Automation.JobQueue.JobQueueYesod
-- Copyright    : Ian-Woo Kim
-- License      : BSD3
-- 
-- Maintainer   : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability    : Experimental
-- Portability  : unknown 
-- 
-- Yesod Server Foundation types
--
----------------------------------------------------


module HEP.Automation.JobQueue.JobQueueYesod where 

import Yesod hiding (update)

import Network.Wai
import Network.Wai.Parse

import qualified Data.Enumerator as E
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

import Data.Aeson.Parser
import Data.Attoparsec

import HEP.Automation.JobQueue.JobType 
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config


import HEP.Automation.JobQueue.QueueServerWork

import Data.Acid 

data JobQueueServer = JobQueueServer { server_acid :: AcidState JobInfoQueue } 

mkYesod "JobQueueServer" [parseRoutes|
/ HomeR GET
/job/#JobNumber JobR GET
/queue QueueR POST
/queuelist QueueListR GET 
/queuelist/unassigned QueueListUnassignedR GET
/queuelist/inprogress QueueListInprogressR GET
/queuelist/finished QueueListFinishedR GET
/job/#JobNumber/assign AssignJobR POST 
|]

instance Yesod JobQueueServer where
  approot _ = ""

type Handler = GHandler JobQueueServer JobQueueServer 

getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [hamlet|Hello World!|]

postQueueR = do 
  liftIO $ putStrLn "postQueueR called" 
  JobQueueServer acid <- getYesod  
  r <- getRequest
  let wr = reqWaiRequest r 
  bs' <- lift E.consume
  let bs = S.concat bs' 
  let parsed = (parseJson bs :: Maybe JobDetail)
  case parsed of 
    Just result -> liftIO $ do 
                     putStrLn $ SC.unpack bs
                     putStrLn $ show result
                     update acid (AddJob result) >>= print  
    Nothing -> liftIO $ do 
                 putStrLn $ "result not parsed well : " 
                 putStrLn $ SC.unpack bs

getJobR n = do
  liftIO $ putStrLn "getJobR called"

getQueueListR = do 
  liftIO $ putStrLn "getQueueListR called" 
  JobQueueServer acid <- getYesod
  r <- liftIO $ query acid QueryAll
  defaultLayoutJson [hamlet| this is html found |] (jsonJobInfoQueue r)

getQueueListUnassignedR = do 
  liftIO $ putStrLn "getQueueListUnassignedR called"
  JobQueueServer acid <- getYesod
  r <- liftIO $ query acid QueryAll
  let f j = jobinfo_status j == Unassigned
      result = filter f (snd r)
  defaultLayoutJson [hamlet| this is html found |] (toAeson result)

getQueueListInprogressR = do 
  liftIO $ putStrLn "getQueueListInprogressR called"
  JobQueueServer acid <- getYesod
  r <- liftIO $ query acid QueryAll 
  let f j = let s = jobinfo_status j
            in (s == Assigned) || (s == BeingCalculated) || (s == BeingTested)
      result = filter f (snd r)
  defaultLayoutJson [hamlet| this is html found |] (toAeson result)

getQueueListFinishedR = do 
  liftIO $ putStrLn "getQueueListFinishedR called"
  JobQueueServer acid <- getYesod
  r <- liftIO $ query acid QueryAll 
  let f j = jobinfo_status j == Finished
      result = filter f (snd r)
  defaultLayoutJson [hamlet| this is html found |] (toAeson result)

postAssignJobR n = do 
  liftIO $ putStrLn "assignJobR called"  
  JobQueueServer acid <- getYesod  
  r <- getRequest
  let wr = reqWaiRequest r 
  bs' <- lift E.consume
  let bs = S.concat bs' 
  let parsed = (parseJson bs :: Maybe ClientConfiguration) 
  case parsed of 
    Just result -> liftIO $ do 
                     putStrLn $ show result
    Nothing -> liftIO $ do 
                 putStrLn $ "result not parsed well : " 
                 S.putStrLn bs