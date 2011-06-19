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

import Yesod

import Network.Wai
import Network.Wai.Parse

import qualified Data.Enumerator as E
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as SC

import Data.Aeson.Parser
import Data.Attoparsec

import HEP.Automation.JobQueue.JobType 
import HEP.Automation.JobQueue.JobJson

data JobQueueServer = JobQueueServer 


mkYesod "JobQueueServer" [$parseRoutes|
/ HomeR GET
/queue QueueR POST
|]

instance Yesod JobQueueServer where
  approot _ = ""

type Handler = GHandler JobQueueServer JobQueueServer 

getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [$hamlet|Hello World!|]


postQueueR = do 
  liftIO $ putStrLn "postQueueR called" 
  r <- getRequest
  let wr = reqWaiRequest r 
  bs' <- lift E.consume
  let bs = S.concat bs' 

  let resultjson = parse json bs

  case resultjson of 
    Done _ rjson -> do 
      let (result :: Maybe EventSet)  = fromAeson rjson
      liftIO $ do 
        putStrLn $ SC.unpack bs 
        putStrLn $ show result -- SC.unpack bs 
    _ -> do 
      liftIO $ do 
        putStrLn $ "result not parsed well : " ++ show resultjson
 
