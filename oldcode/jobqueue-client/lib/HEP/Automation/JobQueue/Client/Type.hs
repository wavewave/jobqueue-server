{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.JobQueue.Client.Type where

import System.Console.CmdArgs

data JobClient = Get    { jobid :: Int, url  :: String } -- config :: FilePath }
               | List   { queuetyp :: String, url :: String } --  config :: FilePath }
               | Start  { config :: FilePath } 
               | StartTest { config :: FilePath }
               | Revert { jobid :: Int, config :: FilePath } 
               | Finish { jobid :: Int, config :: FilePath }
               | Delete { jobid :: Int, config :: FilePath } 
               deriving (Show,Data,Typeable)

get :: JobClient 
get = Get { jobid = 0 &= typ "JOBID" &= argPos 0 
          , url = def &= argPos 1}

list :: JobClient 
list = List { queuetyp = "all" &= typ "QUEUETYP" &= argPos 0 
            , url = def &= argPos 1} 
start :: JobClient 
start = Start { config = "test.conf" }

starttest :: JobClient 
starttest = StartTest { config = "test.conf" }

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
mode = modes [get, list, start, starttest, revert, finish, delete] 