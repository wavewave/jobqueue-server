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
import           Control.Monad.Trans.Resource
import           Data.Acid
import           Data.Aeson as A
import           Data.Attoparsec as P
import qualified Data.ByteString as S
import           Data.Conduit 
import qualified Data.Conduit.List as CL
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
    <title> test 
  <body> 
    <h1> hello world 
|]


defhlet :: Widget
defhlet = [whamlet| <h1> HTML output not supported |]


getListYesodcrudR :: Handler Value -- TypedContent
getListYesodcrudR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  returnJson (Just r)


postUploadYesodcrudR :: Handler Value -- TypedContent
postUploadYesodcrudR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- server_acid <$> getYesod
  wr <- reqWaiRequest <$> getRequest
  bs' <- liftIO $ unfoldM $ do bstr <- requestBody wr      
                               return (if S.null bstr then Nothing else Just bstr)
  liftIO $ print bs'
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
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



handleYesodcrudR :: UUID -> Handler Value -- TypedContent
handleYesodcrudR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getYesodcrudR name
    "PUT" -> putYesodcrudR name
    "DELETE" -> deleteYesodcrudR name
    x -> error ("No such action " ++ show x ++ " in handlerYesodcrudR")

getYesodcrudR :: UUID -> Handler Value -- TypedContent
getYesodcrudR idee = do 
  liftIO $ putStrLn "getYesodcrudR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryYesodcrud idee)
  -- let hlet = [whamlet| <h1> File #{idee}|]
  returnJson (Just r)


putYesodcrudR :: UUID -> Handler Value -- TypedContent
putYesodcrudR idee = do 
  liftIO $ putStrLn "putYesodcrudR called"
  acid <- server_acid <$> getYesod
  wr <- reqWaiRequest <$> getRequest
  bs' <- liftIO $ unfoldM $ do bstr <- requestBody wr 
                               return (if S.null bstr then Nothing else Just bstr)
  let bs = S.concat bs' 
  let parsed = parse json bs 
  liftIO $ print parsed 
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

deleteYesodcrudR :: UUID -> Handler Value -- TypedContent
deleteYesodcrudR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteYesodcrud idee)
  returnJson (Just r)
