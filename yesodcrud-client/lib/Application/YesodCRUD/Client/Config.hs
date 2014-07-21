{-# LANGUAGE OverloadedStrings #-}

module Application.YesodCRUD.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data YesodcrudClientConfiguration = YesodcrudClientConfiguration { 
  yesodcrudServerURL :: String,
  yesodcrudClientURL :: String
} deriving (Show)

getYesodcrudClientConfiguration :: Config -> IO (Maybe YesodcrudClientConfiguration)
getYesodcrudClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  return  (YesodcrudClientConfiguration  <$> s <*> c )
