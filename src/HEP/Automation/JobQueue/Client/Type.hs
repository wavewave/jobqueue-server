{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.JobQueue.Client.Type where

import System.Console.CmdArgs

data JobClient = Get    { jobid :: Int, config :: FilePath }
               | List   { queuetyp :: String, config :: FilePath }
               | Start  { config :: FilePath } 
               | Revert { jobid :: Int, config :: FilePath } 
               | Finish { jobid :: Int, config :: FilePath }
               | Delete { jobid :: Int, config :: FilePath } 
               deriving (Show,Data,Typeable)

get :: JobClient 
get = Get { jobid = 0 &= typ "JOBID" &= argPos 0 
          , config = "test.conf" }
list :: JobClient 
list = List { queuetyp = "all" &= typ "QUEUETYP" &= argPos 0 
            , config = "test.conf" } 
start :: JobClient 
start = Start { config = "test.conf" }

revert :: JobClient 
revert = Revert { jobid = 0 &= typ "JOBID" &= argPos 0 
                , config = "test.conf" }

finish :: JobClient 
finish = Finish { jobid = 0 &= typ "JOBID" &= argPos 0 
                , config = "test.conf" }


delete :: JobClient 
delete = Delete { jobid = 0 &= typ "JOBID" &= argPos 0 
                , config = "test.conf" }


mode :: JobClient 
mode = modes [get, list, start, revert, finish, delete] 