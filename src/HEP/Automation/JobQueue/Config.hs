module HEP.Automation.JobQueue.Config where

data ClientConfiguration = ClientConfiguration { 
  computerName :: String, 
  haveMathematica :: Bool,
  havePBS :: Bool, 
  canMonteCarlo :: Bool, 
  datasetDir :: String
} deriving Show


 



