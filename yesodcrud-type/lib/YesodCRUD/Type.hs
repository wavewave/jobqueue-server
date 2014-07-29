{-# LANGUAGE DeriveDataTypeable, 
             TemplateHaskell, 
             TypeFamilies, 
             TypeSynonymInstances, 
             OverloadedStrings, 
             FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module YesodCRUD.Type where

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

{- 
-- |
instance SafeCopy UUID where 
  putCopy uuid = contain $ safePut (toByteString uuid) 
  getCopy = contain 
            $ maybe (fail "cannot parse UUID") return . fromByteString 
              =<< safeGet

$(deriveSafeCopy 0 'base ''YesodcrudInfo)
-}


-- | 
type YesodcrudInfoRepository = M.Map UUID YesodcrudInfo 


{- 
-- |
addYesodcrud :: YesodcrudInfo -> Update YesodcrudInfoRepository YesodcrudInfo 
addYesodcrud minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\_k _o n -> n) (yesodcrud_uuid minfo) minfo m
  put m'
  return minfo
-}

{- 
-- |
queryYesodcrud :: UUID -> Query YesodcrudInfoRepository (Maybe YesodcrudInfo) 
queryYesodcrud uuid = do 
  m <- ask 
  return (M.lookup uuid m)
-}

{- 
-- |
queryAll :: Query YesodcrudInfoRepository [YesodcrudInfo]
queryAll = do m <- ask   
              return (M.elems m)
-}

{-
-- | 
updateYesodcrud :: YesodcrudInfo -> Update YesodcrudInfoRepository (Maybe YesodcrudInfo)
updateYesodcrud minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_ _ -> Just minfo) (yesodcrud_uuid minfo) m
  put m'
  maybe (return Nothing) (const (return (Just minfo))) r 
-}


{-
-- | 
deleteYesodcrud :: UUID -> Update YesodcrudInfoRepository (Maybe YesodcrudInfo)
deleteYesodcrud uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing
-}

{-
$(makeAcidic ''YesodcrudInfoRepository [ 'addYesodcrud, 'queryYesodcrud, 'queryAll, 'updateYesodcrud, 'deleteYesodcrud] )
-}
