
-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.JobQueue.Config 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
module HEP.Automation.JobQueue.Config where
 
data ClientConfiguration = ClientConfiguration { 
  computerName :: String, 
  haveMathematica :: Bool,
  havePBS :: Bool, 
  canMonteCarlo :: Bool, 
  datasetDir :: String
} deriving Show


 



