{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-} 

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

import           Control.Monad 
import           Control.Monad.Trans.Maybe 
import           Data.Acid 
import           Data.Aeson.Types hiding (parse)
import qualified Data.Aeson.Generic as G
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as M
import           Data.List 
import qualified Data.Text as T
import           Network.Wai
import           Text.Hamlet
import           Yesod hiding (update)
import           Yesod.Form.Jquery
-- 
import HEP.Automation.MadGraph.Util 
import HEP.Automation.EventGeneration.Type
import HEP.Automation.JobQueue.JobType 
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config
import HEP.Storage.WebDAV.Type
--
import HEP.Automation.JobQueue.Server.Type
import HEP.Automation.JobQueue.Server.JobAssign
import Import

data JobQueueServer = JobQueueServer { 
  server_acid :: AcidState JobInfoQueue,
  server_conf :: ServerConfig 
} 


mkYesod "JobQueueServer" [parseRoutes|
/ HomeR GET
/job/#JobNumber JobR 
/revert/#JobNumber RevertR GET
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
  approot = ApprootStatic ""

instance YesodJquery JobQueueServer where
    urlJqueryJs _ = Right "http://ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min.js"



replaceLst :: (Eq a) => [(a,b)] -> [a] -> Maybe [b]
replaceLst assoc lst = mapM (\x -> lookup x assoc) lst

makeTypedContentFromHamletJson :: Handler Html -> Value -> Handler TypedContent
makeTypedContentFromHamletJson hlet j = 
  selectRep $ do provideRep $ hlet
                 provideRep $ return j 


postQueueManyR :: Handler TypedContent 
postQueueManyR =do
  liftIO $ putStrLn "postQueueManyR called"  
  JobQueueServer acid _ <- getYesod  
  parsed <- parseJsonBody
  case parsed of 
    Error str -> makeTypedContentFromHamletJson (return [shamlet| result not parsed well |]) (toJSON str)
    Success idjinfos -> do  
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
          let hlet = [shamlet| 
<html>
  <head>
     <title> No HTML support
  <body> 
     <h1> This page does not have a HTML support 
|] 

          makeTypedContentFromHamletJson (return hlet) $ toJSON ("Success" :: String)
        Nothing -> do 
          makeTypedContentFromHamletJson (return [shamlet| this is html found |]) (toJSON ("Failed" :: String))




getHomeR :: Handler Html 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  return [shamlet|Hello World!|]

handleJobR :: JobNumber -> Handler TypedContent
handleJobR number = do 
  r <- getRequest
  let wr = reqWaiRequest r
  liftIO (putStrLn (show $ requestHeaders wr))
  case requestMethod wr of
    "GET" -> getJobR number
    "PUT" -> putJobR number 
    "DELETE" -> deleteJobR number

deleteJobR :: Int -> Handler TypedContent
deleteJobR n = do 
  liftIO $ putStrLn "deleteJobR called"
  JobQueueServer acid _ <- getYesod
  r <- liftIO $ query acid (QueryJob n) 
  case r of 
    Nothing -> makeTypedContentFromHamletJson (return [shamlet|this is html|]) (toJSON ("No such job" :: String))
    Just _  -> do
      liftIO $ update acid (DeleteJob n) >>= print  
      makeTypedContentFromHamletJson (return [shamlet|success|]) (toJSON ("Delete Succeed" :: String))

getRevertR :: Int -> Handler Html
getRevertR n = do
  liftIO $ putStrLn "getJobR called"
  JobQueueServer acid sconf <- getYesod 
  r <- liftIO $ query acid (QueryJob n)  
  let rstr = case r of 
               Nothing -> Left ("No such job" :: String)
               Just j  -> Right j
  case rstr of 
    Left e -> do  
     let titlestr = "Job " ++ show n ++ " detail" 
     defaultLayout $ do 
       getYesod >>= addScriptEither . urlJqueryJs
       addStylesheetRemote "https://netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
       addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"
       addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
       $(widgetFile "joberror")

    Right jold -> do 
      let j = jold { jobinfo_status = Unassigned }
          jid = jobinfo_id j 

      liftIO $ update acid (UpdateJob jid j)
      let jdet = jobinfo_detail j 
          url = case server_webdav sconf of
                  GlobalURL u -> u 
                  LocalURL u' -> u'
          jremotedir = webdav_remotedir . jobdetail_remotedir $ jdet
          jstatus = show . jobinfo_status $ j 
          jpriority = show . jobinfo_priority $ j 
      case (jobdetail_evset jdet) of 
        EventSet ps param rs -> do 
          let wname = makeRunName ps param rs  
              titlestr = "Job " ++ show n ++ " detail"  
          defaultLayout $ do 
            setTitle (toHtml titlestr)
            getYesod >>= addScriptEither . urlJqueryJs
            addStylesheetRemote "https://netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
            $(widgetFile "job")


getJobR :: Int -> Handler TypedContent
getJobR n = do
  liftIO $ putStrLn "getJobR called"
  JobQueueServer acid sconf <- getYesod 
  r <- liftIO $ query acid (QueryJob n)  
  let rstr = case r of 
               Nothing -> Left ("No such job" :: String)
               Just j  -> Right j
  renderer <- getUrlRenderParams
  let getJobhamlet = case rstr of 
        Left e -> do  
          let titlestr = "Job " ++ show n ++ " detail" 
          defaultLayout $ do
            setTitle (toHtml titlestr)
            getYesod >>= addScriptEither . urlJqueryJs
            addStylesheetRemote "https://netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
            addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"
            addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
            $(widgetFile "joberror")
        Right j -> do 
          let jid = jobinfo_id j 
              jdet = jobinfo_detail j 
              url = case server_webdav sconf of
                      GlobalURL u -> u 
                      LocalURL u' -> u'
              jremotedir = webdav_remotedir . jobdetail_remotedir $ jdet
              jstatus = show . jobinfo_status $ j 
              jpriority = show . jobinfo_priority $ j 
          case (jobdetail_evset jdet) of 
            EventSet ps param rs -> do 
              let wname = makeRunName ps param rs  
                  titlestr = "Job " ++ show n ++ " detail"  
              defaultLayout $ do 
                setTitle (toHtml titlestr)
                getYesod >>= addScriptEither . urlJqueryJs
                addStylesheetRemote "https://netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"
                addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
                $(widgetFile "job")
  makeTypedContentFromHamletJson getJobhamlet (toJSON rstr)

putJobR :: Int -> Handler TypedContent
putJobR n = do 
  liftIO $ putStrLn "putJobR called"
  JobQueueServer acid _ <- getYesod 
  parsed <- parseJsonBody 
  case parsed of 
    Error str -> makeTypedContentFromHamletJson (return [shamlet| this is not html |]) (toJSON (Left str :: Either String JobInfo))
    Success result  -> do 
      liftIO $ do 
        putStrLn $ show result
        update acid (UpdateJob n result) >>= print  
      makeTypedContentFromHamletJson (return [shamlet| this is html found |]) (toJSON (Right result :: Either String JobInfo))


postQueueR :: Int -> Handler ()
postQueueR prior = do 
  liftIO $ putStrLn "postQueueR called" 
  JobQueueServer acid _ <- getYesod  
  parsed <- parseJsonBody 
  case parsed of 
    Error str -> liftIO $ putStrLn $ "result not parsed well : " ++ str
    Success result -> liftIO $ do 
                     putStrLn $ show result
                     if prior == 0 
                       then update acid (AddJob result) >>= print  
                       else update acid (AddJobWithPriority result Urgent) >>= print 

getQueueListR :: Handler TypedContent
getQueueListR = do 
  liftIO $ putStrLn "getQueueListR called" 
  JobQueueServer acid sconf <- getYesod
  let url = server_main sconf 
  r <- liftIO $ query acid QueryAll
  let result = snd r
  makeTypedContentFromHamletJson (hamletListJobs url "all" result) (toJSON result)

getQueueListUnassignedR :: Handler TypedContent
getQueueListUnassignedR = do 
  liftIO $ putStrLn "getQueueListUnassignedR called"
  JobQueueServer acid sconf <- getYesod
  let url = server_main sconf 
  r <- liftIO $ query acid QueryAll
  let f j = jobinfo_status j == Unassigned
      result = filter f (snd r)
  makeTypedContentFromHamletJson (hamletListJobs url "unassigned" result) (toJSON result)

getQueueListInprogressR :: Handler TypedContent
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
  makeTypedContentFromHamletJson (hamletListJobs url "inprogress" result) (toJSON result)

getQueueListFinishedR :: Handler TypedContent
getQueueListFinishedR = do 
  liftIO $ putStrLn "getQueueListFinishedR called"
  JobQueueServer acid sconf <- getYesod
  let url = server_main sconf 
  r <- liftIO $ query acid QueryAll 
  let f j = case jobinfo_status j of 
              Finished _ -> True 
              _ -> False
      result = filter f (snd r)
  makeTypedContentFromHamletJson (hamletListJobs url "finished" result) (toJSON result)

hamletListJobs :: String -> String -> [JobInfo] -> Handler Html
hamletListJobs url str lst = 
  let jobname jdet = 
        case (jobdetail_evset jdet) of 
          EventSet p param r -> makeRunName p param r
      jobtype :: JobInfo -> String 
      jobtype job = case jobinfo_detail job of 
                      EventGen _ _ -> "EventGen"
                      MathAnal _ _ _ -> "Mathematica"
      jobstatusshow :: JobInfo -> String 
      jobstatusshow job = case jobinfo_status job of 
                            Unassigned -> "Unassigned"
                            Assigned _ -> "Assigned"
                            BeingCalculated _ -> "BeingCalculated"
                            BeingTested _ -> "BeingTested"
                            Finished _ -> "Finished"
      assignedclient :: JobInfo -> String 
      assignedclient job = case jobinfo_status job of      
                             Unassigned -> "none"
                             Assigned c -> c
                             BeingCalculated c -> c
                             BeingTested c -> c 
                             Finished c -> c 
  in defaultLayout $ do
       getYesod >>= addScriptEither . urlJqueryJs
       addStylesheetRemote "https://netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"
       addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"
       addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"
       $(widgetFile "listjob")

{- 
[shamlet| 
    <h1> List #{str}
    <table> 
      <tr> 
        <td> id 
        <td> name
        <td> type 
        <td> status
        <td> client
        <td> priority
        <td> dependency
      $forall job <- lst 
        <tr> 
          <td> 
            <a href=#{url}/job/#{jobinfo_id job}> #{jobinfo_id job}
          <td> #{jobname (jobinfo_detail job)}
          <td> #{jobtype job}
          <td> #{jobstatusshow job}
          <td> #{assignedclient job}
          <td> #{show (jobinfo_priority job)}
          <td> #{show (jobinfo_dependency job)}
  |]
-}


postAssignR :: Handler TypedContent
postAssignR = do 
  liftIO $ putStrLn "assignR called"  
  JobQueueServer acid _ <- getYesod  
  parsed <- parseJsonBody 
  case parsed of 
    Error str -> makeTypedContentFromHamletJson (return [shamlet| result not parsed well |]) (toJSON ("result not parsed well :" ++ str))
    Success cc -> do (_,listall) <- liftIO $ query acid QueryAll
                     let priorcomp j1 j2 
                           | jobinfo_priority j1 > jobinfo_priority j2 = LT
                           | jobinfo_priority j1 == jobinfo_priority j2 =
                               compare (jobinfo_id j1) (jobinfo_id j2)
                           | jobinfo_priority j1 < jobinfo_priority j2 = GT
                     let priorityordered = sortBy priorcomp listall 
                     let unassigned = filter (\x->jobinfo_status x == Unassigned) priorityordered 
                     let finished = filter (\x->case jobinfo_status x of {Finished _ -> True ; _ -> False }) priorityordered
                     firstJobAssignment cc (unassigned,finished)


getConfigWebDAVR :: Handler TypedContent 
getConfigWebDAVR = do 
  JobQueueServer _ sconf  <- getYesod  
  let wdav = server_webdav sconf 
  liftIO $ putStrLn "getConfigWebDAVR called" 
  let url = case wdav of 
              GlobalURL u -> u 
              LocalURL u' -> u' 
  let configWebDAVhamlet = do 
        [shamlet|
!!!
<html>
  <head> 
    <title> WebDAV configuration
  <body>   
    <h1> WebDAV configuration
    <h2> WebDAV server is 
    <p>
      <a href=#{url}>  #{url} 
|]
  makeTypedContentFromHamletJson (return configWebDAVhamlet) (toJSON wdav)  
              
jsonJobInfoQueue :: (Int,[JobInfo]) -> Value
jsonJobInfoQueue (lastid,jobinfos) = 
  let lastidjson = toJSON lastid 
      jobinfosjson = toJSON jobinfos
  in  Object $ M.fromList [ ("lastid", lastidjson)
                          , ("map", jobinfosjson) ]


firstJobAssignment :: ClientConfiguration -> ([JobInfo],[JobInfo]) 
                   -> Handler TypedContent  
firstJobAssignment cc (unassigned,finished) = do 
  let r = findFirstJob cc (unassigned,finished)
  case r of 
    Nothing -> do 
      makeTypedContentFromHamletJson (return [shamlet| no such job |]) (toJSON (Left "no compatible job" :: Either String JobInfo)) 
    Just assigned -> do 
      
      makeTypedContentFromHamletJson (return [shamlet| this is html found |]) 
                                     (toJSON (Right assigned :: Either String JobInfo))





