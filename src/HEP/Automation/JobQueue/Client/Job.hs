{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.Client.Job where

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy as L 
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC

import HEP.Automation.JobQueue.Config
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.JobJson

import HEP.Storage.WebDAV.Type

import Data.Aeson.Types
import Data.Aeson.Encode



jobqueueGet :: JobNumber -> IO () 
jobqueueGet jid = do 
  putStrLn "get" 
  manager <- newManager
  requestget <- parseUrl (SC.pack ("http://127.0.0.1:3600/job/" ++ (show jid)))
  let requestgetjson = requestget { 
        requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
      }
  r <- httpLbs requestgetjson manager 
  putStrLn $ show r 

jobqueuePut :: JobInfo -> IO () 
jobqueuePut jinfo = do 
  putStrLn "put" 
  manager <- newManager 
  requesttemp <- parseUrl (SC.pack ("http://127.0.0.1:3600/job/" 
                                   ++ show (jobinfo_id jinfo)))
  let myrequestbody = RequestBodyLBS . encode . toAeson $ jinfo
  let requestput = requesttemp { 
                     method = methodPut, 
                     requestHeaders = [ ("Content-Type", "text/plain") 
                                      , ("Accept", "application/json; charset=utf-8")], 
                     requestBody = myrequestbody 
                   } 
  r <- httpLbs requestput manager 
  putStrLn $ show r 

jobqueueList :: IO () 
jobqueueList = do 
  putStrLn "list"
  manager <- newManager 
  requestget <- parseUrl ("http://127.0.0.1:3600/queuelist")
  let requestgetjson = requestget { 
        requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
      }
  r <- httpLbs requestgetjson manager 
  putStrLn $ show r 

jobqueueUnassigned :: IO () 
jobqueueUnassigned = do 
  putStrLn "jobs unassigned:"
  manager <- newManager 
  requestget <- parseUrl ("http://127.0.0.1:3600/queuelist/unassigned")
  let requestgetjson = requestget { 
        requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
      }
  r <- httpLbs requestgetjson manager 
  putStrLn $ show r 

jobqueueInprogress :: IO ()
jobqueueInprogress = do 
  putStrLn "jobs in progress:"
  manager <- newManager 
  requestget <- parseUrl ("http://127.0.0.1:3600/queuelist/inprogress")
  let requestgetjson = requestget { 
        requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
      }
  r <- httpLbs requestgetjson manager 
  putStrLn $ show r 

jobqueueFinished :: IO ()
jobqueueFinished   = do 
  putStrLn "jobs finished:"
  manager <- newManager 
  requestget <- parseUrl ("http://127.0.0.1:3600/queuelist/finished")
  let requestgetjson = requestget { 
        requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
      }
  r <- httpLbs requestgetjson manager 
  putStrLn $ show r 

jobqueueAssign :: ClientConfiguration -> IO (Maybe JobInfo) 
jobqueueAssign cc = do 
  putStrLn $ "Assign request of job "
  manager <- newManager 
  requesttemp <- parseUrl.SC.pack $ "http://127.0.0.1:3600/assign"
  let ccjson = encode $ toAeson cc
      myrequestbody = RequestBodyLBS ccjson 
      requestpost = requesttemp { 
                      method = methodPost, 
                      requestHeaders = [ ("Content-Type", "text/plain") 
                                       , ("Accept", "application/json; charset=utf-8")], 
                      requestBody = myrequestbody } 
  r <- httpLbs requestpost manager 
  let result = ( parseJson  . SC.concat . C.toChunks .  responseBody ) r :: Maybe (Either String JobInfo) 
  case result of 
    Just (Right jinfo) -> do putStrLn "Job assigned"
                             putStrLn (show jinfo)
                             let newjob = jinfo { jobinfo_status = Assigned } 
                             jobqueuePut newjob 
                             return (Just newjob)
    Just (Left msg)    -> do putStrLn "Error message from server" 
                             putStrLn msg
                             return Nothing
    Nothing            -> do putStrLn "Parsing failed"
                             return Nothing 

startjob :: JobInfo -> IO () 
startjob _ = return ()

getWebDAVInfo :: IO ()
getWebDAVInfo = do 
  r <- getJsonFromServer "/config/webdav" 
  case r of 
    Nothing -> putStrLn "Nothing"
    Just value -> do 
      let c = fromAeson value :: Maybe WebDAVServer
      putStrLn (show c)
--  putStrLn (show (statusCode r))
--  putStrLn (show (responseHeaders r))
--  putStrLn (show (responseBody r))




getJsonFromServer :: String -> IO (Maybe Value)
getJsonFromServer api = do 
  manager <- newManager
  requestget <- parseUrl (SC.pack ("http://127.0.0.1:3600" ++ api))
  let requestgetjson = requestget { 
        requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
      } 
  r <- httpLbs requestgetjson manager 
  if statusCode r == 200 
    then return . parseJson . SC.concat . C.toChunks . responseBody $ r
    else return Nothing

 