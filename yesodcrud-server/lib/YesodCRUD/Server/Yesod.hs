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

module YesodCRUD.Server.Yesod where 

import           Control.Applicative
import           Control.Monad.Loops
-- import           Data.Acid
import           Data.Aeson as A
import           Data.Attoparsec.ByteString as P
import qualified Data.ByteString as S
import           Data.Maybe
import qualified Data.Text as T
import           Data.UUID
import           Database.Persist
import           Database.Persist.Sqlite
import           Network.Wai
import           Yesod hiding (update)
-- 
import           YesodCRUD.Type
import           YesodCRUD.Server.Type



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
  dbfile <- server_db <$> getYesod
  runSqlite dbfile $ do 
    runMigration migrateCrud
    results <- selectList [] [] 
    (returnJson . catMaybes) 
      (map (fromCrudInfo . entityVal) results :: [Maybe YesodcrudInfo])

-- |
postCreateR :: Handler Value
postCreateR = do
  dbfile <- server_db <$> getYesod
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
          runSqlite dbfile $ do
            runMigration migrateCrud
            insert (toCrudInfo minfo) 
            returnJson (Just minfo)
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
  dbfile <- server_db <$> getYesod
  liftIO $ print dbfile
  runSqlite dbfile $ do 
    runMigration migrateCrud
    mresult <- (getBy . UniqueUUID . T.pack . toString) idee
    case mresult of 
      Nothing -> lift (returnJson (Nothing :: Maybe YesodcrudInfo))
      Just ett -> (lift . returnJson . fromCrudInfo . entityVal) ett

-- | 
putUUIDR :: UUID -> Handler Value
putUUIDR idee = do 
  dbfile <- server_db <$> getYesod
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
            then  
              runSqlite dbfile $ do
                runMigration migrateCrud
                mresult <- (getBy . UniqueUUID . T.pack . toString) idee

                case mresult of 
                  Nothing -> lift (returnJson (Nothing :: Maybe YesodcrudInfo))
                  Just ett -> do 
                    update (entityKey ett) [ CrudInfoName =. yesodcrud_name minfo ]
                    lift (returnJson (Just minfo)) 
                -- lift (returnJson (Nothing :: Maybe YesodcrudInfo))
            else do 
              liftIO $ putStrLn "yesodcrudname mismatched"
              returnJson (Nothing :: Maybe YesodcrudInfo)
        Error err -> do 
          liftIO $ putStrLn err 
          returnJson (Nothing :: Maybe YesodcrudInfo)
 

-- | 
deleteUUIDR :: UUID -> Handler Value
deleteUUIDR idee = undefined {- do 
  acid <- return.server_acid =<< getYesod
  r <- liftIO $ update acid (DeleteYesodcrud idee)
  returnJson (Just r)
  -}
