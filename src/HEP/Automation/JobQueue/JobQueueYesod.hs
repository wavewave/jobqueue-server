{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}


{-# LANGUAGE EmptyDataDecls #-}

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



data JobQueueServer = JobQueueServer 

mkYesod "JobQueueServer" [$parseRoutes|
/ HomeR GET
|]

instance Yesod JobQueueServer where
  approot _ = ""

type Handler = GHandler JobQueueServer JobQueueServer 

getHomeR = do 
  liftIO $ putStrLn "getHomeR called"
  defaultLayout [$hamlet|Hello World!|]


