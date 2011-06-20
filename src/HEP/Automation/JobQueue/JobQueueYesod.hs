{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}


{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

----------------------------------------------------
--
-- Module       : HEP.Automation.JobQueue.JobQueueYesod
-- Copyright    : Ian-Woo Kim
-- License      : BSD3
-- 
-- Maintainer   : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability    : Experimental
-- Portability  : unknown 
-- 
-- Yesod Server Foundation types
--
----------------------------------------------------


module HEP.Automation.JobQueue.JobQueueYesod where 

import Yesod hiding (update)

import Network.Wai
import Network.Wai.Parse

import qualified Data.Enumerator as E
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

import Data.Aeson.Parser
import Data.Attoparsec

import HEP.Automation.JobQueue.JobType 
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.JobQueue.JobQueue

import HEP.Automation.JobQueue.QueueServerWork

import Data.Acid 

data JobQueueServer = JobQueueServer { server_acid :: AcidState JobInfoQueue } 


mkYesod "JobQueueServer" [parseRoutes|
/ HomeR GET
/queue QueueR POST
/queuelist QueueListR GET 
|]

instance Yesod JobQueueServer where
  approot _ = ""

type Handler = GHandler JobQueueServer JobQueueServer 

getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [hamlet|Hello World!|]


postQueueR = do 
  liftIO $ putStrLn "postQueueR called" 
  JobQueueServer acid <- getYesod  
  r <- getRequest
  let wr = reqWaiRequest r 
  bs' <- lift E.consume
  let bs = S.concat bs' 

  let parsed = parseJobDetail bs 

  case parsed of 
    Just result -> liftIO $ do 
                     putStrLn $ SC.unpack bs
                     putStrLn $ show result
                     update acid (AddJob result) >>= print  

    Nothing -> liftIO $ do 
                 putStrLn $ "result not parsed well : " 
                 putStrLn $ SC.unpack bs

getQueueListR = do 
  liftIO $ putStrLn "getQueueListR called" 
  JobQueueServer acid <- getYesod
  liftIO $ do r <- query acid QueryAll
              putStrLn (show r) 