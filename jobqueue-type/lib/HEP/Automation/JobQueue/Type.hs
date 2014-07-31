{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HEP.Automation.JobQueue.Type where

import           Control.Applicative 
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.Data
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text.Encoding as E
import           Data.Typeable
import           Data.UUID
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

-- | 
data YesodcrudInfo = YesodcrudInfo { 
  yesodcrud_uuid :: UUID, 
  yesodcrud_name :: T.Text
} deriving (Show,Typeable,Data)

-- |
instance FromJSON UUID where
  parseJSON x = do r <- return . fromString . C.unpack . E.encodeUtf8 =<< parseJSON x
                   case r of 
                     Nothing -> fail ("UUID parsing failed " ++ show x )
                     Just uuid -> return uuid 

-- | 
instance ToJSON UUID where
  toJSON = toJSON . E.decodeUtf8 . C.pack . toString 

instance FromJSON YesodcrudInfo where
  parseJSON (Object v) = YesodcrudInfo <$>  v .: "uuid" <*> v .: "name"

-- |
instance ToJSON YesodcrudInfo where
  toJSON (YesodcrudInfo uuid name) = object [ "uuid" .= uuid , "name" .= name ] 


share [mkPersist sqlSettings, mkMigrate "migrateCrud"] [persistLowerCase|
CrudInfo 
  uuid T.Text
  name T.Text
  UniqueUUID uuid
  deriving Show
|]

fromCrudInfo :: CrudInfo -> Maybe YesodcrudInfo
fromCrudInfo (CrudInfo uuidtxt nametxt) = 
    let muuid = fromString (T.unpack uuidtxt)
    in  fmap (\x -> YesodcrudInfo x nametxt) muuid

toCrudInfo :: YesodcrudInfo -> CrudInfo
toCrudInfo YesodcrudInfo {..} = CrudInfo (T.pack (toString yesodcrud_uuid)) yesodcrud_name

-- | 
type YesodcrudInfoRepository = M.Map UUID YesodcrudInfo 


