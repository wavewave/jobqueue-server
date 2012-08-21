{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application.YesodCRUD.Server.Yesod where 

import           Control.Applicative
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
  -- approot = ""
  maximumContentLength _ _ = 100000000

{-instance RenderMessage YesodcrudServer FormMessage where
  renderMessage _ _ = defaultFormMessage -}


getHomeR :: Handler RepHtml 
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


defhlet :: GWidget s m ()
defhlet = [whamlet| <h1> HTML output not supported |]


getListYesodcrudR :: Handler RepHtmlJson
getListYesodcrudR = do 
  liftIO $ putStrLn "getQueueListR called" 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))


postUploadYesodcrudR :: Handler RepHtmlJson
postUploadYesodcrudR = do 
  liftIO $ putStrLn "postQueueR called" 
  acid <- server_acid <$> getYesod
  wr <- reqWaiRequest <$> getRequest
  bs' <- liftIO $ runResourceT (requestBody wr $$ CL.consume)
  let bs = S.concat bs' 
  let parsed = parse json bs 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result YesodcrudInfo) of 
        Success minfo -> do 
          r <- liftIO $ update acid (AddYesodcrud minfo)
          liftIO $ print (Just r)
          liftIO $ print (A.toJSON (Just r))
          defaultLayoutJson defhlet (A.toJSON (Just r))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe YesodcrudInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe YesodcrudInfo))
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe YesodcrudInfo))



handleYesodcrudR :: UUID -> Handler RepHtmlJson
handleYesodcrudR name = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getYesodcrudR name
    "PUT" -> putYesodcrudR name
    "DELETE" -> deleteYesodcrudR name
    x -> error ("No such action " ++ show x ++ " in handlerYesodcrudR")

getYesodcrudR :: UUID -> Handler RepHtmlJson
getYesodcrudR idee = do 
  liftIO $ putStrLn "getYesodcrudR called"
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryYesodcrud idee)
  liftIO $ putStrLn $ show r 
  let hlet = [whamlet| <h1> File #{idee}|]
  defaultLayoutJson hlet (A.toJSON (Just r))


putYesodcrudR :: UUID -> Handler RepHtmlJson
putYesodcrudR idee = do 
  liftIO $ putStrLn "putYesodcrudR called"
  acid <- server_acid <$> getYesod
  wr <- reqWaiRequest <$> getRequest
  bs' <- liftIO $ runResourceT (requestBody wr $$ CL.consume)
  let bs = S.concat bs'
  let parsed = parse json bs 
  liftIO $ print parsed 
  case parsed of 
    Done _ parsedjson -> do 
      case (A.fromJSON parsedjson :: A.Result YesodcrudInfo) of 
        Success minfo -> do 
          if idee == yesodcrud_uuid minfo
            then do r <- liftIO $ update acid (UpdateYesodcrud minfo)
                    defaultLayoutJson defhlet (A.toJSON (Just r))
            else do liftIO $ putStrLn "yesodcrudname mismatched"
                    defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe YesodcrudInfo))
        Error err -> do 
          liftIO $ putStrLn err 
          defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe YesodcrudInfo))
    Fail _ ctxts err -> do 
      liftIO $ putStrLn (concat ctxts++err)
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe YesodcrudInfo))
         
    Partial _ -> do 
      liftIO $ putStrLn "partial" 
      defaultLayoutJson defhlet (A.toJSON (Nothing :: Maybe YesodcrudInfo))

deleteYesodcrudR :: UUID -> Handler RepHtmlJson
deleteYesodcrudR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteYesodcrud idee)
  liftIO $ putStrLn $ show r 
  defaultLayoutJson defhlet (A.toJSON (Just r))
