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

module Application.YesodCRUD.Server.Yesod where 

import           Control.Applicative
import           Control.Monad.Loops
import           Data.Acid
import           Data.Aeson as A
import           Data.Attoparsec.ByteString as P
import qualified Data.ByteString as S
import           Data.UUID
import           Network.Wai
import           Yesod hiding (update)
-- 
import           Application.YesodCRUD.Type
import           Application.YesodCRUD.Server.Type



mkYesod "YesodcrudServer" [parseRoutes|
/ HomeR GET
/listyesodcrud  ListYesodcrudR GET
/uploadyesodcrud  UploadYesodcrudR POST
/yesodcrud/#UUID YesodcrudR 
|]

instance Yesod YesodcrudServer where
  maximumContentLength _ _ = Just 100000000


getHomeR :: Handler Html 
getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [whamlet|
!!!
<html>
  <head> 
    <title> yesod crud
  <body> 
    <h1> YESOD CRUD EXAMPLE
|]


getListYesodcrudR :: Handler Value
getListYesodcrudR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  returnJson (Just r)


postUploadYesodcrudR :: Handler Value
postUploadYesodcrudR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- server_acid <$> getYesod
  wr <- reqWaiRequest <$> getRequest
  bs' <- liftIO $ unfoldM $ do bstr <- requestBody wr      
                               return (if S.null bstr then Nothing else Just bstr)
  let bs = S.concat bs' 
  let parsed = parseOnly json bs 
  case parsed of 
    Left err -> do liftIO $ putStrLn err 
                   returnJson (Nothing :: Maybe YesodcrudInfo)
    Right parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result YesodcrudInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddYesodcrud minfo)
          returnJson (Just r)
        Error err -> do 
          liftIO $ putStrLn err 
          returnJson (Nothing :: Maybe YesodcrudInfo)

{-  
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result YesodcrudInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddYesodcrud minfo)
          returnJson (Just r)
        Error err -> do 
          liftIO $ putStrLn err 
          returnJson (Nothing :: Maybe YesodcrudInfo)
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      returnJson (Nothing :: Maybe YesodcrudInfo)
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      returnJson (Nothing :: Maybe YesodcrudInfo)
-}


handleYesodcrudR :: UUID -> Handler Value
handleYesodcrudR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getYesodcrudR name
    "PUT" -> putYesodcrudR name
    "DELETE" -> deleteYesodcrudR name
    x -> error ("No such action " ++ show x ++ " in handlerYesodcrudR")

getYesodcrudR :: UUID -> Handler Value
getYesodcrudR idee = do 
  liftIO $ putStrLn "getYesodcrudR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryYesodcrud idee)
  returnJson (Just r)


putYesodcrudR :: UUID -> Handler Value
putYesodcrudR idee = do 
  liftIO $ putStrLn "putYesodcrudR called"
  acid <- server_acid <$> getYesod
  wr <- reqWaiRequest <$> getRequest
  bs' <- liftIO $ unfoldM $ do bstr <- requestBody wr 
                               return (if S.null bstr then Nothing else Just bstr)
  let bs = S.concat bs' 
  let parsed = parseOnly json bs 
  case parsed of 
    Left err -> do liftIO $ putStrLn err 
                   returnJson (Nothing :: Maybe YesodcrudInfo)
    Right parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result YesodcrudInfo) of 
        Success minfo -> do 
          if idee == yesodcrud_uuid minfo
            then do r <- liftIO $ update acid (UpdateYesodcrud minfo)
                    returnJson (Just r)
            else do liftIO $ putStrLn "yesodcrudname mismatched"
                    returnJson (Nothing :: Maybe YesodcrudInfo)
        Error err -> do 
          liftIO $ putStrLn err 
          returnJson (Nothing :: Maybe YesodcrudInfo)
 
  {- 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result YesodcrudInfo) of 
        Success minfo -> do 
          if idee == yesodcrud_uuid minfo
            then do r <- liftIO $ update acid (UpdateYesodcrud minfo)
                    returnJson (Just r)
            else do liftIO $ putStrLn "yesodcrudname mismatched"
                    returnJson (Nothing :: Maybe YesodcrudInfo)
        Error err -> do 
          liftIO $ putStrLn err 
          returnJson (Nothing :: Maybe YesodcrudInfo)
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      returnJson (Nothing :: Maybe YesodcrudInfo)
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      returnJson (Nothing :: Maybe YesodcrudInfo)
   -}

deleteYesodcrudR :: UUID -> Handler Value
deleteYesodcrudR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteYesodcrud idee)
  returnJson (Just r)
