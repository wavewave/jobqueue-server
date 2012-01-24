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

import Control.Monad
import Control.Monad.Trans.Maybe

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

import Data.Aeson.Types hiding (parse)

import HEP.Automation.MadGraph.Util 

import HEP.Automation.JobQueue.JobType 
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config

import qualified Data.HashMap.Strict as M
import Data.List 

import Data.Acid 

import HEP.Automation.JobQueue.Server.Type
import HEP.Automation.JobQueue.Server.JobAssign

import HEP.Storage.WebDAV.Type


data JobQueueServer = JobQueueServer { 
  server_acid :: AcidState JobInfoQueue,
  server_conf :: ServerConfig 
} 

mkYesod "JobQueueServer" [parseRoutes|
/ HomeR GET
/job/#JobNumber JobR 
/queue/#Int QueueR POST
/queuemany QueueManyR POST 
/queuelist QueueListR GET 
/queuelist/unassigned QueueListUnassignedR GET
/queuelist/inprogress QueueListInprogressR GET
/queuelist/finished QueueListFinishedR GET
/assign AssignR POST 
/config/webdav ConfigWebDAVR GET
|]

instance Yesod JobQueueServer where
  approot _ = ""

-- type Handler = GHandler JobQueueServer JobQueueServer 

replaceLst :: (Eq a) => [(a,b)] -> [a] -> Maybe [b]
replaceLst assoc lst = mapM (\x -> lookup x assoc) lst

makeRepHtmlJsonFromHamletJson :: HtmlUrl (Route JobQueueServer) -> Value -> Handler RepHtmlJson
makeRepHtmlJsonFromHamletJson hlet j = do
  RepHtml rhtml <- hamletToRepHtml hlet 
  RepJson json <- jsonToRepJson j 
  return (RepHtmlJson rhtml json) 

postQueueManyR :: Handler RepHtmlJson 
postQueueManyR =do
  liftIO $ putStrLn "postQueueManyR called"  
  JobQueueServer acid _ <- getYesod  
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = (parseJson bs :: Either String ManyJobInfo)
  case parsed of 
    Left str -> makeRepHtmlJsonFromHamletJson [hamlet| result not parsed well |] (toAeson str)
    Right idjinfos -> do  
      let uploadjob :: (Int,JobInfo) -> Handler (Int,Int)
          uploadjob (i,jinfo) = do 
            let priority = jobinfo_priority jinfo
                detail = jobinfo_detail jinfo
            (i',_) <- liftIO $ update acid (AddJobWithPriority detail priority)
            return (i,i')
      let foldfunc :: [(Int,Int)] -> (Int,JobInfo) ->  Handler [(Int,Int)]
          foldfunc acc x = do 
            r <- uploadjob x 
            return (r:acc)
      idsublst :: [(Int,Int)] <- foldM foldfunc [] idjinfos
      let updatejob :: [(Int,Int)] -> (Int,JobInfo) -> MaybeT Handler (EventResult UpdateJob)
          updatejob lst (i,jinfo) = do
            i' <- MaybeT . return $ lookup i lst   
            let dep = jobinfo_dependency jinfo
            dep' <- MaybeT . return $ replaceLst lst dep
            let jinfo' = jinfo { jobinfo_id = i', jobinfo_dependency = dep' }
            liftIO $ update acid (UpdateJob i' jinfo')  
      ur <- runMaybeT $ mapM (updatejob idsublst) idjinfos
      case ur of 
        Just _ -> do 
          let hlet = [hamlet| 
<html>
  <head>
     <title> No HTML support
  <body> 
     <h1> This page does not have a HTML support 
|] 

          makeRepHtmlJsonFromHamletJson hlet $ toAeson ("Success" :: String)
        Nothing -> do 
          makeRepHtmlJsonFromHamletJson [hamlet| this is html found |] (toAeson ("Failed" :: String))


getHomeR :: Handler RepHtml 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  hamletToRepHtml [hamlet|Hello World!|]

handleJobR :: JobNumber -> Handler RepHtmlJson
handleJobR number = do 
  r <- getRequest
  let wr = reqWaiRequest r
  liftIO (putStrLn (show $ requestHeaders wr))
  case requestMethod wr of
    "GET" -> getJobR number
    "PUT" -> putJobR number 
    "DELETE" -> deleteJobR number

deleteJobR :: Int -> Handler RepHtmlJson
deleteJobR n = do 
  liftIO $ putStrLn "deleteJobR called"
  JobQueueServer acid _ <- getYesod
  r <- liftIO $ query acid (QueryJob n) 
  case r of 
    Nothing -> makeRepHtmlJsonFromHamletJson [hamlet|this is html|] (toAeson ("No such job" :: String))
    Just _  -> do
      liftIO $ update acid (DeleteJob n) >>= print  
      makeRepHtmlJsonFromHamletJson [hamlet|success|] (toAeson ("Delete Succeed" :: String))

getJobR :: Int -> Handler RepHtmlJson
getJobR n = do
  liftIO $ putStrLn "getJobR called"
  JobQueueServer acid sconf <- getYesod 
  r <- liftIO $ query acid (QueryJob n)  
  let rstr = case r of 
               Nothing -> Left ("No such job" :: String)
               Just j  -> Right j
  let getJobhamlet = case rstr of 
        Left e -> do  
          let titlestr = "Job " ++ show n ++ " detail" 
          [hamlet|
             <html> 
               <head> 
                 <title>#{titlestr}
               <body> 
                 <h1> Job #{n} 
                 <p 
                    #{e}
          |]
        Right j -> do 
          let jid = jobinfo_id j 
              jdet = jobinfo_detail j 
              url = webdav_server_url . server_webdav $ sconf 
              jremotedir = webdav_remotedir . jobdetail_remotedir $ jdet
              jstatus = show . jobinfo_status $ j 
              jpriority = show . jobinfo_priority $ j 
          case (jobdetail_evset jdet) of 
            EventSet ps rs -> do 
              let wname = makeRunName ps rs  
                  titlestr = "Job " ++ show n ++ " detail"  
              [hamlet| 
                 <html> 
                   <head> 
                     <title>#{titlestr}
                   <body>
                     <h1> Job #{n} 
                     <ul 
                       <li> job id = #{jid} 
                       <li> job name = #{wname} 
                       <li> job status = #{jstatus}
                       <li> job priority = #{jpriority}  
                       <li> job remote dir = 
                         <a href=#{url}/#{jremotedir}> #{jremotedir} 
                       <li> job detail = #{show jdet} 
              |]       
  makeRepHtmlJsonFromHamletJson getJobhamlet (toAeson rstr)

putJobR :: Int -> Handler RepHtmlJson
putJobR n = do 
  liftIO $ putStrLn "putJobR called"
  JobQueueServer acid _ <- getYesod 
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = (parseJson bs :: Either String JobInfo)
  case parsed of 
    Left str -> makeRepHtmlJsonFromHamletJson [hamlet| this is not html |] (toAeson ("Fail" :: String))
    Right result -> do 
      liftIO $ do 
        putStrLn $ SC.unpack bs
        putStrLn $ show result
        update acid (UpdateJob n result) >>= print  
      makeRepHtmlJsonFromHamletJson [hamlet| this is html found |] (toAeson ("Success" :: String))

postQueueR :: Int -> Handler ()
postQueueR prior = do 
  liftIO $ putStrLn "postQueueR called" 
  JobQueueServer acid _ <- getYesod  
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = (parseJson bs :: Either String JobDetail)
  case parsed of 
    Left str -> liftIO $ putStrLn $ "result not parsed well : " ++ str
    Right result -> liftIO $ do 
                     putStrLn $ SC.unpack bs
                     putStrLn $ show result
                     if prior == 0 
                       then update acid (AddJob result) >>= print  
                       else update acid (AddJobWithPriority result Urgent) >>= print 

getQueueListR :: Handler RepHtmlJson
getQueueListR = do 
  liftIO $ putStrLn "getQueueListR called" 
  JobQueueServer acid sconf <- getYesod
  let url = server_main sconf 
  r <- liftIO $ query acid QueryAll
  let result = snd r
  makeRepHtmlJsonFromHamletJson (hamletListJobs url "all" result) (toAeson result)

getQueueListUnassignedR :: Handler RepHtmlJson
getQueueListUnassignedR = do 
  liftIO $ putStrLn "getQueueListUnassignedR called"
  JobQueueServer acid sconf <- getYesod
  let url = server_main sconf 
  r <- liftIO $ query acid QueryAll
  let f j = jobinfo_status j == Unassigned
      result = filter f (snd r)
  makeRepHtmlJsonFromHamletJson (hamletListJobs url "unassigned" result) (toAeson result)

getQueueListInprogressR :: Handler RepHtmlJson
getQueueListInprogressR = do 
  liftIO $ putStrLn "getQueueListInprogressR called"
  JobQueueServer acid sconf <- getYesod
  let url = server_main sconf 
  r <- liftIO $ query acid QueryAll 
  let f j = case jobinfo_status j of
              Assigned _ -> True
              BeingCalculated _ -> True 
              BeingTested _ -> True
              _ -> False 
      result = filter f (snd r)
  makeRepHtmlJsonFromHamletJson (hamletListJobs url "inprogress" result) (toAeson result)

getQueueListFinishedR :: Handler RepHtmlJson
getQueueListFinishedR = do 
  liftIO $ putStrLn "getQueueListFinishedR called"
  JobQueueServer acid sconf <- getYesod
  let url = server_main sconf 
  r <- liftIO $ query acid QueryAll 
  let f j = case jobinfo_status j of 
              Finished _ -> True 
              _ -> False
      result = filter f (snd r)
  makeRepHtmlJsonFromHamletJson (hamletListJobs url "finished" result) (toAeson result)

hamletListJobs :: String -> String -> [JobInfo] -> HtmlUrl (Route JobQueueServer)
hamletListJobs url str lst = do 
  let jobname jdet = 
        case (jobdetail_evset jdet) of 
          EventSet p r -> makeRunName p r
  let jobtype :: JobInfo -> String 
      jobtype job = case jobinfo_detail job of 
                      EventGen _ _ -> "EventGen"
                      MathAnal _ _ _ -> "Mathematica"
  let jobstatusshow :: JobInfo -> String 
      jobstatusshow job = case jobinfo_status job of 
                            Unassigned -> "Unassigned"
                            Assigned _ -> "Assigned"
                            BeingCalculated _ -> "BeingCalculated"
                            BeingTested _ -> "BeingTested"
                            Finished _ -> "Finished"
  let assignedclient :: JobInfo -> String 
      assignedclient job = case jobinfo_status job of      
                             Unassigned -> "none"
                             Assigned c -> c
                             BeingCalculated c -> c
                             BeingTested c -> c 
                             Finished c -> c 
  [hamlet| 
    <h1> List #{str}
    <table 
      <tr 
        <td> id 
        <td> name
        <td> type 
        <td> status
        <td> client
        <td> priority
        <td> dependency
      $forall job <- lst 
        <tr 
          <td> 
            <a href=#{url}/job/#{jobinfo_id job}> #{jobinfo_id job}
          <td> #{jobname (jobinfo_detail job)}
          <td> #{jobtype job}
          <td> #{jobstatusshow job}
          <td> #{assignedclient job}
          <td> #{show (jobinfo_priority job)}
          <td> #{show (jobinfo_dependency job)}
  |]



postAssignR :: Handler RepHtmlJson
postAssignR = do 
  liftIO $ putStrLn "assignR called"  
  JobQueueServer acid _ <- getYesod  
  _ <- getRequest
  bs' <- lift EL.consume
  let bs = S.concat bs' 
  let parsed = (parseJson bs :: Either String ClientConfiguration) 
  case parsed of 
    Left str -> makeRepHtmlJsonFromHamletJson [hamlet| result not parsed well |] (toAeson ("result not parsed well :" ++ str))
    Right cc-> do (_,listall) <- liftIO $ query acid QueryAll
                  let priorcomp j1 j2 
                        | jobinfo_priority j1 > jobinfo_priority j2 = LT
                        | jobinfo_priority j1 == jobinfo_priority j2 =
                           compare (jobinfo_id j1) (jobinfo_id j2)
                        | jobinfo_priority j1 < jobinfo_priority j2 = GT
                  let priorityordered = sortBy priorcomp listall 
                  let unassigned = filter (\x->jobinfo_status x == Unassigned) priorityordered 
                  let finished = filter (\x->case jobinfo_status x of {Finished _ -> True ; _ -> False }) priorityordered
                  firstJobAssignment cc (unassigned,finished)


getConfigWebDAVR :: Handler RepHtmlJson 
getConfigWebDAVR = do 
  JobQueueServer _ sconf  <- getYesod  
  let wdav = server_webdav sconf 
  liftIO $ putStrLn "getConfigWebDAVR called" 
  let url = webdav_server_url wdav 
  let configWebDAVhamlet = do 
        [hamlet|
!!!
<html>
  <head> 
    <title> WebDAV configuration
  <body>   
    <h1> WebDAV configuration
    <h2> WebDAV server is 
    <p
      <a href=#{url}>  #{url} 
|]
  makeRepHtmlJsonFromHamletJson configWebDAVhamlet (toAeson wdav)  
              
jsonJobInfoQueue :: (Int,[JobInfo]) -> Value
jsonJobInfoQueue (lastid,jobinfos) = 
  let lastidjson = toAeson lastid 
      jobinfosjson = toAeson jobinfos
  in  Object $ M.fromList [ ("lastid", lastidjson)
                          , ("map", jobinfosjson) ]


firstJobAssignment :: ClientConfiguration -> ([JobInfo],[JobInfo]) 
                   -> GGHandler JobQueueServer JobQueueServer (E.Iteratee SC.ByteString IO) RepHtmlJson 
firstJobAssignment cc (unassigned,finished) = do 
  let r = findFirstJob cc (unassigned,finished)
  case r of 
    Nothing -> do 
      liftIO $ putStrLn "No Compatible Job!"
      makeRepHtmlJsonFromHamletJson [hamlet| no such job |] (toAeson (Left "no compatible job" :: Either String JobInfo)) 
    Just assigned -> do 
      liftIO $ putStrLn "Job Found!"
      liftIO $ putStrLn (show assigned) 
      makeRepHtmlJsonFromHamletJson [hamlet| this is html found |] (toAeson (Right assigned :: Either String JobInfo))





