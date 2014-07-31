{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module HEP.Automation.JobQueue.Client.Job where

import System.FilePath

import Control.Monad

import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC

import HEP.Automation.JobQueue.Config
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.JobJson

import HEP.Storage.WebDAV.Type

-- import Data.Aeson.Generic hiding (encode)
import Data.Aeson.Types
import Data.Aeson.Encode
import Data.Aeson.Parser
import qualified Data.Attoparsec as P
import Data.Text.Encoding 

import Control.Monad.Trans

type Url = String 

jobqueueGet :: Url -> JobNumber -> IO (Result JobInfo) 
jobqueueGet url jid = do 
  putStrLn "get" 
  r <- getJsonFromServer url ("job/" ++ show jid)
  return r 

-- |   

jobqueuePut :: Url -> JobInfo -> IO (Maybe JobInfo)
jobqueuePut url jinfo = do 
  putStrLn "put" 
  withManager $ \manager -> do
    requesttemp <- parseUrl (url </> "job" 
                                 </> show (jobinfo_id jinfo))
    let myrequestbody = RequestBodyLBS . encode . toJSON $ jinfo
    let requestput = requesttemp { 
                       method = methodPut, 
                       requestHeaders = [ ("Content-Type", "text/plain") 
                                        , ("Accept", "application/json; charset=utf-8")], 
                       requestBody = myrequestbody 
                     } 
    r <- httpLbs requestput manager 
    liftIO ( putStrLn $ show r )
    return (Just jinfo)

-- | 

jobqueueDelete :: Url -> Int -> IO ()
jobqueueDelete url jid = do 
  putStrLn "delete" 
  withManager $ \manager -> do
    requesttemp <- parseUrl (url </> "job" </> show jid )
    let requestdel = requesttemp { 
                       method = methodDelete, 
                       requestHeaders = [ ("Content-Type", "text/plain") 
                                        , ("Accept", "application/json; charset=utf-8")]
                     } 
    r <- httpLbs requestdel manager 
    liftIO (putStrLn $ show r )

jobqueueList :: Url -> IO () 
jobqueueList url = do 
  putStrLn "list"
  withManager $ \manager -> do
    requestget <- parseUrl (url </> "queuelist")
    let requestgetjson = requestget { 
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
        }
    r <- httpLbs requestgetjson manager 
    liftIO (putStrLn $ show r)


-- | 

jobqueueUnassigned :: Url -> IO ()
jobqueueUnassigned = jobqueueStatus "queuelist/unassigned"

-- | 

jobqueueInprogress :: Url -> IO ()
jobqueueInprogress = jobqueueStatus "queuelist/inprogress"

-- | 

jobqueueFinished :: Url -> IO ()
jobqueueFinished = jobqueueStatus "queuelist/finished"

-- | 

jobqueueStatus :: String -> Url -> IO ()
jobqueueStatus cmd url = do 
  putStrLn cmd
  (r :: Result [JobInfo]) <- getJsonFromServer url cmd
  case r of 
    Error err -> putStrLn err
    Success jinfos -> forM_ jinfos $ \x -> do putStrLn "-------------" 
                                              putStrLn (show x)

-- | 

jobqueueAssign :: Url -> ClientConfiguration -> IO (Maybe JobInfo) 
jobqueueAssign url cc = do 
  putStrLn $ "Assign request of job "
  withManager $ \manager -> do
    requesttemp <- parseUrl (url </> "assign")
    let ccjson = encode $ toJSON cc
        myrequestbody = RequestBodyLBS ccjson 
        requestpost = requesttemp { 
                        method = methodPost, 
                        requestHeaders = [ ("Content-Type", "text/plain") 
                                         , ("Accept", "application/json; charset=utf-8")], 
                        requestBody = myrequestbody } 
    r <- httpLbs requestpost manager 
    let result = ( parseJson . SC.concat . C.toChunks .  responseBody ) r :: Result JobInfo 
    case result of
      Error err -> do {liftIO (putStrLn ("error msg from server : " ++ err)); return Nothing }
      Success info -> return (Just info)


-- | 

confirmAssignment :: Url -> String -> JobInfo -> IO (Maybe JobInfo)
confirmAssignment url cname jinfo = do  
  putStrLn "try confirmation"
  putStrLn (show jinfo)
  case jobinfo_status jinfo of 
    Unassigned -> do 
      let newjob = jinfo { jobinfo_status = Assigned cname } 
      jobqueuePut url newjob 
    _ -> return Nothing 

-- | 

backToUnassigned :: Url -> JobInfo -> IO (Maybe JobInfo)
backToUnassigned url jinfo = changeStatus url jinfo Unassigned 

-- | 

makeFinished :: Url -> JobInfo -> IO (Maybe JobInfo)
makeFinished url jinfo = changeStatus url jinfo (Finished "forcefully")

-- | 

changeStatus :: Url -> JobInfo -> JobStatus -> IO (Maybe JobInfo)
changeStatus url jinfo status = do 
  let newjob = jinfo { jobinfo_status = status } 
  jobqueuePut url newjob

-- |

getWebDAVInfo :: Url -> IO (Result WebDAVServer)
getWebDAVInfo url = getJsonFromServer url "config/webdav" 

{-
  case r of 
    Nothing -> return Nothing
    Just value -> do 
      let c = fromJSON value :: Result WebDAVServer
      case c of 
        Error str -> return Nothing
        Success c' -> return (Just c')
-}

-- | 

getJsonFromServer :: (FromJSON a) => Url -> String -> IO (Result a)
getJsonFromServer url api = do 
  withManager $ \manager -> do
    requestget <- parseUrl (url </> api)
    let requestgetjson = requestget { 
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
        } 
    r <- httpLbs requestgetjson manager 
    if responseStatus r == ok200 
      then (return . parseJson . SC.concat . C.toChunks . responseBody) r
      else fail $ url ++ " is not working"

-- | 
 
parseJson :: (FromJSON a) => SC.ByteString -> Result a
parseJson bs =
  let resultjson = P.parse json bs
  in case resultjson of 
       P.Done _ rjson -> fromJSON rjson
       _            -> fail "parsing failed"
