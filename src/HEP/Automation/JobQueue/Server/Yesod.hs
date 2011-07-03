{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

----------------------------------------------------
--
-- Module       : HEP.Automation.JobQueue.Server.Yesod
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


module HEP.Automation.JobQueue.Server.Yesod where 

import Yesod hiding (update)

import Network.Wai
import Network.Wai.Parse

import qualified Data.Enumerator as E
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

import Data.Aeson.Types hiding (parse)
import Data.Aeson.Parser
import Data.Attoparsec

import HEP.Automation.JobQueue.JobType 
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config

import qualified Data.Map as M
import qualified Data.IntMap as IM 

import Data.Acid 

import HEP.Automation.JobQueue.Server.Type
import HEP.Automation.JobQueue.Server.Work

data JobQueueServer = JobQueueServer { 
  server_acid :: AcidState JobInfoQueue,
  server_conf :: ServerConfig 
} 

mkYesod "JobQueueServer" [parseRoutes|
/ HomeR GET
/job/#JobNumber JobR 
/queue QueueR POST
/queuelist QueueListR GET 
/queuelist/unassigned QueueListUnassignedR GET
/queuelist/inprogress QueueListInprogressR GET
/queuelist/finished QueueListFinishedR GET
/assign AssignR POST 
/config/webdav ConfigWebDAVR GET
|]

instance Yesod JobQueueServer where
  approot _ = ""

type Handler = GHandler JobQueueServer JobQueueServer 



getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [hamlet|Hello World!|]

handleJobR :: JobNumber -> Handler RepHtmlJson
handleJobR number = do 
  r <- getRequest
  let wr = reqWaiRequest r
  liftIO (putStrLn (show $ requestHeaders wr))
  case requestMethod wr of
    "GET" -> getJobR number
    "PUT" -> putJobR number 
    "DELETE" -> deleteJobR number

deleteJobR n = do 
  liftIO $ putStrLn "deleteJobR called"
  JobQueueServer acid _ <- getYesod
  r <- liftIO $ query acid (QueryJob n) 
  case r of 
    Nothing -> defaultLayoutJson [hamlet|this is html|] (toAeson ("No such job" :: String))
    Just j  -> do
      liftIO $ update acid (DeleteJob n) >>= print  
      defaultLayoutJson [hamlet|this is html|] (toAeson ("Delete Succeed" :: String))

getJobR n = do
  liftIO $ putStrLn "getJobR called"
  JobQueueServer acid _ <- getYesod 
  r <- liftIO $ query acid (QueryJob n)  
  let rstr = case r of 
               Nothing -> Left ("No such job" :: String)
               Just j  -> Right j
  defaultLayoutJson [hamlet| this is html found |] (toAeson rstr)

putJobR n = do 
  liftIO $ putStrLn "putJobR called"
  JobQueueServer acid _ <- getYesod 
  r <- getRequest
  let wr = reqWaiRequest r 
  bs' <- lift E.consume
  let bs = S.concat bs' 
  let parsed = (parseJson bs :: Maybe JobInfo)
  case parsed of 
    Just result -> do 
      liftIO $ do 
        putStrLn $ SC.unpack bs
        putStrLn $ show result
        update acid (UpdateJob n result) >>= print  
      defaultLayoutJson [hamlet| this is html found |] (toAeson ("Success" :: String))
    Nothing -> do
      liftIO $ do 
        putStrLn $ "result not parsed well : " 
        putStrLn $ SC.unpack bs
      defaultLayoutJson [hamlet| this is not html |] (toAeson ("Fail" :: String))

postQueueR = do 
  liftIO $ putStrLn "postQueueR called" 
  JobQueueServer acid _ <- getYesod  
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


getQueueListR = do 
  liftIO $ putStrLn "getQueueListR called" 
  JobQueueServer acid _ <- getYesod
  r <- liftIO $ query acid QueryAll
  defaultLayoutJson [hamlet| this is html found |] (jsonJobInfoQueue r)

getQueueListUnassignedR = do 
  liftIO $ putStrLn "getQueueListUnassignedR called"
  JobQueueServer acid _ <- getYesod
  r <- liftIO $ query acid QueryAll
  let f j = jobinfo_status j == Unassigned
      result = filter f (snd r)
  defaultLayoutJson [hamlet| this is html found |] (toAeson result)

getQueueListInprogressR = do 
  liftIO $ putStrLn "getQueueListInprogressR called"
  JobQueueServer acid _ <- getYesod
  r <- liftIO $ query acid QueryAll 
  let f j = let s = jobinfo_status j
            in (s == Assigned) || (s == BeingCalculated) || (s == BeingTested)
      result = filter f (snd r)
  defaultLayoutJson [hamlet| this is html found |] (toAeson result)

getQueueListFinishedR = do 
  liftIO $ putStrLn "getQueueListFinishedR called"
  JobQueueServer acid _ <- getYesod
  r <- liftIO $ query acid QueryAll 
  let f j = jobinfo_status j == Finished
      result = filter f (snd r)
  defaultLayoutJson [hamlet| this is html found |] (toAeson result)

postAssignR = do 
  liftIO $ putStrLn "assignR called"  
  JobQueueServer acid _ <- getYesod  
  r <- getRequest
  bs' <- lift E.consume
  let bs = S.concat bs' 
  let parsed = (parseJson bs :: Maybe ClientConfiguration) 
  case parsed of 
    Just cc -> do listall <- liftIO $ query acid QueryAll
                  let unassigned = filter (\x->jobinfo_status x == Unassigned) (snd listall)
                  firstJobAssignment cc unassigned
    Nothing -> do liftIO $ do 
                    putStrLn $ "result not parsed well : " 
                    S.putStrLn bs
                  defaultLayoutJson [hamlet| result not parsed well |] (toAeson (Left "result not parsed well" :: Either String JobInfo))


getConfigWebDAVR :: Handler RepHtmlJson 
getConfigWebDAVR = do 
  JobQueueServer _ sconf  <- getYesod  
  let wdav = server_webdav sconf 
  liftIO $ putStrLn "getConfigWebDAVR called" 
  defaultLayoutJson [hamlet| this is html |] (toAeson wdav) 
              
jsonJobInfoQueue :: (Int,[JobInfo]) -> Value
jsonJobInfoQueue (lastid,jobinfos) = 
  let lastidjson = toAeson lastid 
      jobinfosjson = toAeson jobinfos
  in  Object $ M.fromList [ ("lastid", lastidjson)
                          , ("map", jobinfosjson) ]

checkJobCompatibility :: ClientConfiguration -> JobInfo -> Bool 
checkJobCompatibility (ClientConfiguration cname math pbs montecarlo) jobinfo =
  case jobinfo_detail jobinfo of 
    EventGen _ _ -> montecarlo
    MathAnal _ _ -> math || pbs 



firstJobAssignment :: ClientConfiguration -> [JobInfo] 
                   -> GGHandler JobQueueServer JobQueueServer (E.Iteratee SC.ByteString IO) RepHtmlJson 
firstJobAssignment cc jobinfos = 
    let compatible = filter (checkJobCompatibility cc) jobinfos
    in  if null compatible 
        then do 
          liftIO $ putStrLn "No Compatible Job!"
          defaultLayoutJson [hamlet| no such job |] (toAeson (Left "no compatible job" :: Either String JobInfo)) 
        else do 
          let assignedCandidate = head compatible
          liftIO $ putStrLn "Job Found!"
          liftIO $ putStrLn (show assignedCandidate) 
          defaultLayoutJson [hamlet| this is html found |] (toAeson (Right assignedCandidate :: Either String JobInfo))



