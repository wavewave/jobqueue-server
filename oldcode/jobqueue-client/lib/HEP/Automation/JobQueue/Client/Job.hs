{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module HEP.Automation.JobQueue.Client.Job where


import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Aeson.Types
import Data.Aeson.Encode
import Data.Aeson.Parser
import qualified Data.Attoparsec as P
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Data.Text.Encoding 
import Network.HTTP.Types hiding (statusCode)
import Network.HTTP.Conduit
import System.FilePath
--
import HEP.Automation.EventGeneration.Config
import HEP.Automation.JobQueue.Config
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.JobJson
import HEP.Storage.WebDAV.Type
--

data SendMethod = MethodPUT | MethodPOST 
                deriving (Show,Eq,Ord)

newtype URL = URL {unURL :: String}

-- type (URL Url) = String 

-- | get job by number
jobqueueGet :: URL -> JobNumber -> IO (Either String JobInfo)
jobqueueGet url jid = join <$> getJsonFromServer url ("job/" ++ show jid) 

-- | put new content to job
jobqueuePut :: URL -> JobInfo -> IO (Either String JobInfo)
jobqueuePut url jinfo = join <$> sendJson MethodPUT url ("job" </> show (jobinfo_id jinfo)) jinfo

{-  do 
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
-}

-- | delete job
jobqueueDelete :: URL -> Int -> IO ()
jobqueueDelete (URL url) jid = do 
  withManager $ \manager -> do
    requesttemp <- parseUrl (url </> "job" </> show jid )
    let requestdel = requesttemp { 
                       method = methodDelete, 
                       requestHeaders = [ ("Content-Type", "text/plain") 
                                        , ("Accept", "application/json; charset=utf-8")]
                     } 
    r <- httpLbs requestdel manager 
    liftIO (putStrLn $ show r )

-- | assign a job to the client
jobqueueAssign :: URL -> ClientConfiguration -> IO (Either String JobInfo) 
jobqueueAssign url cc  = join <$> sendJson MethodPOST url "assign" cc

-- | 
confirmAssignment :: URL -> String -> JobInfo -> IO (Either String JobInfo)
confirmAssignment url cname jinfo = case jobinfo_status jinfo of 
                                      Unassigned -> jobqueuePut url (jinfo { jobinfo_status = Assigned cname })
                                      _ -> return (Left "job is already assigned to somebody")

-- | 
backToUnassigned :: URL -> JobInfo -> IO (Either String JobInfo)
backToUnassigned url jinfo = changeStatus url jinfo Unassigned 

-- | 
makeFinished :: URL -> JobInfo -> IO (Either String JobInfo)
makeFinished url jinfo = changeStatus url jinfo (Finished "forcefully")

-- | 
changeStatus :: URL -> JobInfo -> JobStatus -> IO (Either String JobInfo)
changeStatus url jinfo status = jobqueuePut url (jinfo { jobinfo_status = status })

-- |
getWebDAVInfo :: URL -> IO (Either String URLtype)
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
getJsonFromServer :: (FromJSON a) => URL -> String -> IO (Either String a)
getJsonFromServer (URL url) api = do 
  withManager $ \manager -> do
    requestget <- parseUrl (url </> api)
    let requestgetjson = requestget { 
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
        } 
    r <- httpLbs requestgetjson manager 
    if responseStatus r == ok200 
      then do
        let jsonstr = (C.toStrict . responseBody) r
        case parseJson jsonstr of
          Success result -> return (Right result)
          Error err -> return (Left err)
      else return (Left (url ++ " is not working"))

-- | 
sendJson :: (ToJSON a, FromJSON b) => SendMethod -> URL -> String -> a -> IO (Either String b)
sendJson method (URL url) api obja = do 
  withManager $ \manager -> do
    requesttemp <- parseUrl (url </> api)
    let objajson = encode $ toJSON obja
        myrequestbody = RequestBodyLBS objajson 
        requestpost = requesttemp { method = case method of 
                                               MethodPUT  -> methodPut
                                               MethodPOST -> methodPost
                                  , requestHeaders = [ ("Content-Type", "text/plain") 
                                  , ("Accept", "application/json; charset=utf-8")]
                                  , requestBody = myrequestbody } 
    r <- httpLbs requestpost manager
    if responseStatus r == ok200 
      then do
        let jsonstr = (C.toStrict . responseBody) r
        liftIO $ SC.putStrLn jsonstr
        case parseJson jsonstr of
          Success result -> return (Right result)
          Error err -> return (Left err)
      else return (Left (url ++ " is not working"))


{-
    let result = ( parseJson . SC.concat . C.toChunks .  responseBody ) r :: Result JobInfo 
    case result of
      Error err -> do {liftIO (putStrLn ("error msg from server : " ++ err)); return Nothing }
      Success info -> return (Just info)
-}

-- | 
parseJson :: (FromJSON a) => SC.ByteString -> Result a
parseJson bs =
  let resultjson = P.parse json bs
  in case resultjson of 
       P.Done _ rjson -> fromJSON rjson
       _            -> fail "parsing failed"
