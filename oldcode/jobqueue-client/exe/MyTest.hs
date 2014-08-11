module Main where

import System.Environment

import HEP.Automation.EventGeneration.Config
import HEP.Automation.JobQueue.Client.Phase 
import HEP.Automation.JobQueue.Client.Job 
import HEP.Automation.JobQueue.Config

ccgen name =ClientConfiguration { computerName = name, haveMathematica = True, havePBS=True,  canMonteCarlo = True, datasetDir = "tes" }

main :: IO ()
main = do
  args <- getArgs
  let fp = args !! 0
  r <- getConfig fp
  case r of
    Nothing -> putStrLn "cannot parse config"
    Just ec -> do
      let cc = ccgen (evgen_computerName ec)
      startWaitPhase (cc,ec) (URL "http://ianwookim.org:3600") 3 10
