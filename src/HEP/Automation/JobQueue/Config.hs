module HEP.Automation.JobQueue.Config where

import HEP.Config.Parse
import Control.Monad.Identity
import Control.Applicative
import Text.Parsec


data ClientConfiguration = ClientConfiguration { 
  computerName :: String, 
  haveMathematica :: Bool,
  havePBS :: Bool
} deriving Show

yesNo :: String -> Bool 
yesNo "Yes" = True
yesNo "No"  = False

clientConfigurationParse :: ParsecT String () Identity ClientConfiguration
clientConfigurationParse = 
  ClientConfiguration <$> (oneFieldInput "computerName")
                      <*> (oneFieldInput "haveMathematica" >>= return . yesNo)
                      <*> (oneFieldInput "havePBS" >>= return . yesNo)

 



