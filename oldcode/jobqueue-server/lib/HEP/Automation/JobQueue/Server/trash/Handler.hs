{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable, 
             MultiParamTypeClasses, TypeFamilies, FlexibleContexts,  
             FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

----------------------------------------------------
--
-- Module       : HEP.Automation.JobQueue.Server.Handler
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

module HEP.Automation.JobQueue.Server.Handler where

import HEP.Automation.JobQueue.Server.Yesod
import HEP.Automation.JobQueue.Server.Work

mkYesod "JobQueueServer" [parseRoutes|
/ HomeR GET
/job/#JobNumber JobR 
/queue QueueR POST
/queuelist QueueListR GET 
/queuelist/unassigned QueueListUnassignedR GET
/queuelist/inprogress QueueListInprogressR GET
/queuelist/finished QueueListFinishedR GET
/assign AssignR POST 
|]

