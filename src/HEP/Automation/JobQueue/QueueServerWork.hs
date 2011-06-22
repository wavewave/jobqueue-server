{-# LANGUAGE OverloadedStrings #-}

module HEP.Automation.JobQueue.QueueServerWork where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L 


import Data.Aeson.Types hiding (parse)
import Data.Aeson.Parser
import Data.Attoparsec

import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.JobQueue.Config

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

  