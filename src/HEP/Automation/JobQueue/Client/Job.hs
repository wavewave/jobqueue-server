{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module HEP.Automation.JobQueue.Client.Job where

import System.FilePath

import Control.Monad

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC

import HEP.Automation.JobQueue.Config
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.JobJson

import HEP.Storage.WebDAV.Type

import Data.Aeson.Types
import Data.Aeson.Encode

type Url = String 

jobqueueGet :: Url -> JobNumber -> IO (Either String JobInfo) 
jobqueueGet url jid = do 
  putStrLn "get" 
  r <- getJsonFromServer url ("job/" ++ show jid)
  putStrLn $ show r
  case r of 
    Nothing -> return (Left "jobqueueGet got Nothing")
    Just x  -> case fromAeson x of 
                 Nothing -> return (Left "fromAeson in jobqueueGet error")
                 Just e  -> return e
  

jobqueuePut :: Url -> JobInfo -> IO (Maybe JobInfo)
jobqueuePut url jinfo = do 
  putStrLn "put" 
  withManager $ \manager -> do -- <- newManager 
    requesttemp <- parseUrl (url </> "job" 
                                 </> show (jobinfo_id jinfo))
    let myrequestbody = RequestBodyLBS . encode . toAeson $ jinfo
    let requestput = requesttemp { 
                       method = methodPut, 
                       requestHeaders = [ ("Content-Type", "text/plain") 
                                        , ("Accept", "application/json; charset=utf-8")], 
                       requestBody = myrequestbody 
                     } 
    r <- httpLbs requestput manager 
    putStrLn $ show r 
    return (Just jinfo)

jobqueueDelete :: Url -> Int -> IO ()
jobqueueDelete url jid = do 
  putStrLn "delete" 
  withManager $ \manager -> do -- <- newManager 
    requesttemp <- parseUrl (url </> "job" </> show jid )
    let requestdel = requesttemp { 
                       method = methodDelete, 
                       requestHeaders = [ ("Content-Type", "text/plain") 
                                        , ("Accept", "application/json; charset=utf-8")]
                     } 
    r <- httpLbs requestdel manager 
    putStrLn $ show r 

jobqueueList :: Url -> IO () 
jobqueueList url = do 
  putStrLn "list"
  withManager $ \manager -> do -- <- newManager 
    requestget <- parseUrl (url </> "queuelist")
    let requestgetjson = requestget { 
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
        }
    r <- httpLbs requestgetjson manager 
    putStrLn $ show r 

jobqueueUnassigned :: Url -> IO () 
jobqueueUnassigned url = do 
  putStrLn "jobs unassigned:"
  r <- getJsonFromServer url "queuelist/unassigned"
  let (result :: Maybe [JobInfo]) = fromAeson =<< r 
  case result of 
    Just jinfos -> forM_ jinfos $ 
                    \x-> do {putStrLn "-------------" ; putStrLn (show x)}
    Nothing     -> do putStrLn "Parsing failed"

jobqueueInprogress :: Url -> IO ()
jobqueueInprogress url = do 
  putStrLn "jobs in progress:"
  r <- getJsonFromServer url "queuelist/inprogress"
  let (result :: Maybe [JobInfo]) = fromAeson =<< r 
  case result of 
    Just jinfos -> forM_ jinfos $ 
                    \x-> do {putStrLn "-------------" ; putStrLn (show x)}
    Nothing     -> do putStrLn "Parsing failed"

jobqueueFinished :: Url -> IO ()
jobqueueFinished url = do 
  putStrLn "jobs finished:"
  r <- getJsonFromServer url "queuelist/finished"
  let (result :: Maybe [JobInfo]) = fromAeson =<< r 
  case result of 
    Just jinfos -> forM_ jinfos $ 
                    \x-> do {putStrLn "-------------" ; putStrLn (show x)}
    Nothing     -> do putStrLn "Parsing failed"

jobqueueAssign :: Url -> ClientConfiguration -> IO (Maybe JobInfo) 
jobqueueAssign url cc = do 
  putStrLn $ "Assign request of job "
  withManager $ \manager -> do -- <- newManager 
    requesttemp <- parseUrl (url </> "assign")
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
      Just (Right jinfo) -> do return (Just jinfo)  

      Just (Left msg)    -> do putStrLn "Error message from server" 
                               putStrLn msg
                               return Nothing
      Nothing            -> do putStrLn "Parsing failed"
                               return Nothing 

confirmAssignment :: Url -> String -> JobInfo -> IO (Maybe JobInfo)
confirmAssignment url cname jinfo = do  
  putStrLn "try confirmation"
  putStrLn (show jinfo)
  case jobinfo_status jinfo of 
    Unassigned -> do 
      let newjob = jinfo { jobinfo_status = Assigned cname } 
      jobqueuePut url newjob 
    _ -> return Nothing 

backToUnassigned :: Url -> JobInfo -> IO (Maybe JobInfo)
backToUnassigned url jinfo = changeStatus url jinfo Unassigned 

changeStatus :: Url -> JobInfo -> JobStatus -> IO (Maybe JobInfo)
changeStatus url jinfo status = do 
  let newjob = jinfo { jobinfo_status = status } 
  jobqueuePut url newjob

getWebDAVInfo :: Url -> IO (Maybe WebDAVServer)
getWebDAVInfo url = do 
  r <- getJsonFromServer url "config/webdav" 
  case r of 
    Nothing -> return Nothing
    Just value -> do 
      let c = fromAeson value :: Maybe WebDAVServer
      return c


getJsonFromServer :: Url -> String -> IO (Maybe Value)
getJsonFromServer url api = do 
  withManager $ \manager -> do
    requestget <- parseUrl (url </> api)
    let requestgetjson = requestget { 
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
        } 
    r <- httpLbs requestgetjson manager 
    if statusCode r == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return Nothing


