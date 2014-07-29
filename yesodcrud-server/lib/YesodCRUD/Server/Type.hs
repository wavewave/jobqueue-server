{-# LANGUAGE OverloadedStrings #-}

module YesodCRUD.Server.Type where

import Data.Text.Encoding as E
import Data.UUID
import qualified Data.ByteString.Char8 as C
-- import Yesod.Dispatch
import Text.Blaze
import YesodCRUD.Type
-- import Debug.Trace 
import Data.Acid
import Web.PathPieces

instance PathPiece UUID where
  fromPathPiece = fromString . C.unpack . E.encodeUtf8
  toPathPiece = E.decodeUtf8 . C.pack . toString 

instance ToMarkup UUID where
  toMarkup = toMarkup . toString 

data YesodcrudServer = YesodcrudServer {
  server_acid :: AcidState YesodcrudInfoRepository
}
