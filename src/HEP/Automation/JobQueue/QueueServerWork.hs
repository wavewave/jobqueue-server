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

import qualified Data.Map as M

parseJobDetail :: S.ByteString -> Maybe JobDetail
parseJobDetail bs =
  let resultjson = parse json bs
  in case resultjson of 
       Done _ rjson -> fromAeson rjson
       _            -> Nothing 
 

jsonJobInfoQueue :: (Int,[JobInfo]) -> Value
jsonJobInfoQueue (lastid,jobinfos) = 
  let lastidjson = toAeson lastid 
      jobinfosjson = toAeson jobinfos
  in  Object $ M.fromList [ ("lastid", lastidjson)
                          , ("map", jobinfosjson) ]


