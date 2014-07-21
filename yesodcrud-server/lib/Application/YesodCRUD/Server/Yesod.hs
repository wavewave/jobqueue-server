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
/            HomeR   GET
/list        ListR   GET 
/create      CreateR POST
/id/#UUID    UUIDR 
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

-- | 
getListR :: Handler Value
getListR = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid QueryAll
  returnJson (Just r)

-- |
postCreateR :: Handler Value
postCreateR = do 
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

-- | 
handleUUIDR :: UUID -> Handler Value
handleUUIDR uuid = do
  wr <- return.reqWaiRequest =<< getRequest
  case requestMethod wr of 
    "GET" -> getUUIDR uuid
    "PUT" -> putUUIDR uuid
    "DELETE" -> deleteUUIDR uuid
    x -> error ("No such action " ++ show x ++ " in handleUUIDR")

-- |
getUUIDR :: UUID -> Handler Value
getUUIDR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ query acid (QueryYesodcrud idee)
  returnJson (Just r)

-- | 
putUUIDR :: UUID -> Handler Value
putUUIDR idee = do 
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
 
-- | 
deleteUUIDR :: UUID -> Handler Value
deleteUUIDR idee = do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteYesodcrud idee)
  returnJson (Just r)
