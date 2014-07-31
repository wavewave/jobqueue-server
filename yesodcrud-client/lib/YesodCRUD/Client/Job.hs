{-# LANGUAGE OverloadedStrings #-}

module YesodCRUD.Client.Job where

import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Data.Aeson.Types
import Data.Aeson.Encode as E
import Data.Aeson.Parser
import qualified Data.Attoparsec as A
import qualified Data.Text as T
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.HTTP.Conduit

import System.Directory 
import System.FilePath
import Unsafe.Coerce

import YesodCRUD.Client.Config
import YesodCRUD.Type
import Data.UUID
import Data.UUID.V5
import qualified Data.ByteString as B
import Data.Time.Clock

type Url = String 

nextUUID :: YesodcrudClientConfiguration -> IO UUID
nextUUID mc = do 
  let c = yesodcrudClientURL mc 
  t <- getCurrentTime 
  return . generateNamed namespaceURL . B.unpack . SC.pack $ c ++ "/" ++ show t 

startCreate :: YesodcrudClientConfiguration -> T.Text -> IO () 
startCreate mc name = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = yesodcrudServerURL mc 
  uuid <- nextUUID mc
  let info = YesodcrudInfo { yesodcrud_uuid = uuid , yesodcrud_name = name } 
  response <- yesodcrudToServer url ("create") methodPost info
  putStrLn $ show response 


startGet :: YesodcrudClientConfiguration -> String -> IO () 
startGet mc idee = do 
  putStrLn $"get " ++ idee
  let url = yesodcrudServerURL mc 
  r <- jsonFromServer url ("id" </> idee) methodGet
  putStrLn $ show r 


startPut :: YesodcrudClientConfiguration 
         -> String  -- ^ yesodcrud idee
         -> T.Text  -- ^ yesodcrud name 
         -> IO () 
startPut mc idee name = do 
  cwd <- getCurrentDirectory
  let url = yesodcrudServerURL mc 
      info = case fromString idee of 
               Nothing -> error "strange in startPut" 
               Just idee' -> YesodcrudInfo { yesodcrud_uuid = idee', yesodcrud_name = name }
  response <- yesodcrudToServer url ("id" </> idee) methodPut info
  putStrLn $ show response 


startDelete :: YesodcrudClientConfiguration -> String -> IO () 
startDelete mc idee = do 
  let url = yesodcrudServerURL mc 
  r <- jsonFromServer url ("id" </> idee) methodDelete
  putStrLn $ show r 


startGetList :: YesodcrudClientConfiguration -> IO () 
startGetList mc = do 
  putStrLn "getlist: "
  let url = yesodcrudServerURL mc 
  r <- jsonFromServer url ("list") methodGet
  putStrLn $ show r 


jsonFromServer :: Url -> String -> Method -> IO (Either String (Result Value))
jsonFromServer url api mthd = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let requestjson = request { 
          method = mthd,
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] } 
    r <- httpLbs requestjson manager 
    if statusCode (responseStatus r) == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode (responseStatus r))) 

yesodcrudToServer :: Url -> String -> Method -> YesodcrudInfo -> IO (Either String (Result Value))
yesodcrudToServer url api mthd mi = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let mijson = E.encode (toJSON mi)
        myrequestbody = RequestBodyLBS mijson 
    let requestjson = request 
          { method = mthd
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    r <- httpLbs requestjson manager 
    if statusCode (responseStatus r) == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode (responseStatus r))) 

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = trace (SC.unpack bs) $ A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "parseJson" 
