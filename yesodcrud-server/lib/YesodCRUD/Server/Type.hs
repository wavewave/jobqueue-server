{-# LANGUAGE OverloadedStrings #-}

module YesodCRUD.Server.Type where

-- import Data.Acid
import qualified Data.ByteString.Char8 as C
import           Data.Text.Encoding as E
import qualified Data.Text as T
import           Data.UUID
-- import           System.FilePath
import           Text.Blaze
import           Web.PathPieces
-- import           YesodCRUD.Type

instance PathPiece UUID where
  fromPathPiece = fromString . C.unpack . E.encodeUtf8
  toPathPiece = E.decodeUtf8 . C.pack . toString 

instance ToMarkup UUID where
  toMarkup = toMarkup . toString 

data YesodcrudServer = YesodcrudServer {
  server_db :: T.Text
  -- server_acid :: AcidState YesodcrudInfoRepository
} 
