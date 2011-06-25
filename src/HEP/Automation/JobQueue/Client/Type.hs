{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.JobQueue.Client.Type where

import System.IO
import System.Console.CmdArgs

data JobClient = Get    { jobid :: Int, config :: FilePath }
               | List   { queuetyp :: String, config :: FilePath }
               | Assign { config :: FilePath } 
               deriving (Show,Data,Typeable)

get :: JobClient 
get = Get { jobid = 0 &= typ "JOBID" &= argPos 0 
          , config = "test.conf" }
list :: JobClient 
list = List { queuetyp = "all" &= typ "QUEUETYP" &= argPos 0 
            , config = "test.conf" } 
assign :: JobClient 
assign = Assign { config = "test.conf" }

mode = modes [get, list, assign] 