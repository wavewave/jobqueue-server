module MyTest where

import HEP.Automation.EventGeneration.Config
import HEP.Automation.JobQueue.Client.Phase 
import HEP.Automation.JobQueue.Client.Job 
import HEP.Automation.JobQueue.Config

ccgen name =ClientConfiguration { computerName = name, haveMathematica = True, havePBS=True,  canMonteCarlo = True, datasetDir = "tes" }


{- 
main = do 
  Right x <- jobqueueGet (URL "http://localhost:3600") 4
  backToUnassigned (URL "http://localhost:3600") x
-}

start fp = do
  r <- getConfig fp
  case r of
    Nothing -> putStrLn "cannot parse config"
    Just ec -> do
      let cc = ccgen (evgen_computerName ec)
      startWaitPhase (cc,ec) (URL "http://ianwookim.org:3600") 3 10
