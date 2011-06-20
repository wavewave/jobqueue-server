module HEP.Automation.JobQueue.QueueServerWork where

import qualified Data.ByteString as S

import Data.Aeson.Parser
import Data.Attoparsec

import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.JobJson

parseJobDetail :: S.ByteString -> Maybe JobDetail
parseJobDetail bs =
  let resultjson = parse json bs
  in case resultjson of 
       Done _ rjson -> fromAeson rjson
       _            -> Nothing 
 


