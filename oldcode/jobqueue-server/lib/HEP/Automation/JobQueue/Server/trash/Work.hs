{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

module HEP.Automation.JobQueue.Server.Work where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L 
import qualified Data.Enumerator as E
import qualified Data.ByteString.Char8 as SC



import Data.Aeson.Types hiding (parse)
import Data.Aeson.Parser
import Data.Attoparsec



import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.JobQueue.Config



import HEP.Automation.JobQueue.Server.Yesod

import Yesod hiding (update)


import qualified Data.Map as M
import qualified Data.IntMap as IM 

jsonJobInfoQueue :: (Int,[JobInfo]) -> Value
jsonJobInfoQueue (lastid,jobinfos) = 
  let lastidjson = toAeson lastid 
      jobinfosjson = toAeson jobinfos
  in  Object $ M.fromList [ ("lastid", lastidjson)
                          , ("map", jobinfosjson) ]

checkJobCompatibility :: ClientConfiguration -> JobInfo -> Bool 
checkJobCompatibility (ClientConfiguration cname math pbs montecarlo) jobinfo =
  case jobinfo_detail jobinfo of 
    EventGen _ -> montecarlo
    MathAnal _ -> math || pbs 



firstJobAssignment :: ClientConfiguration -> [JobInfo] 
                   -> GGHandler JobQueueServer JobQueueServer (E.Iteratee SC.ByteString IO) RepHtmlJson 
firstJobAssignment cc jobinfos = 
    let compatible = filter (checkJobCompatibility cc) jobinfos
    in  if null compatible 
        then do 
          liftIO $ putStrLn "No Compatible Job!"
          defaultLayoutJson [hamlet| no such job |] (toAeson (Left "no compatible job" :: Either String JobInfo)) 
        else do 
          let assignedCandidate = head compatible
          liftIO $ putStrLn "Job Found!"
          liftIO $ putStrLn (show assignedCandidate) 
          defaultLayoutJson [hamlet| this is html found |] (toAeson (Right assignedCandidate :: Either String JobInfo))


  