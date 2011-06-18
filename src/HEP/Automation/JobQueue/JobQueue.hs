{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

----------------------------------------------------
--
-- Module       : HEP.Automation.JobQueue.JobQueue
-- Copyright    : Ian-Woo Kim
-- License      : BSD3
-- 
-- Maintainer   : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability    : Experimental
-- Portability  : unknown 
-- 
-- JobQueue Type
--
----------------------------------------------------

module HEP.Automation.JobQueue.JobQueue where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Typeable

import Data.Acid


import HEP.Automation.JobQueue.JobType

import Prelude hiding (length)
import Data.Sequence
import Data.SafeCopy


data JobInfo = JobInfo {
  jobinfo_id     :: Int, 
  jobinfo_detail :: JobDetail, 
  jobinfo_status :: JobStatus
} deriving Typeable

data JobDetail = EventGen EventSet | MathAnal EventSet 

data JobStatus = Unassigned 
               | Assigned 
               | BeingCalculated
               | BeingTested
               | Finished

instance SafeCopy JobStatus where
  putCopy Unassigned      = contain $ safePut (0 :: Int) 
  putCopy Assigned        = contain $ safePut (1 :: Int) 
  putCopy BeingCalculated = contain $ safePut (2 :: Int)
  putCopy BeingTested     = contain $ safePut (3 :: Int)
  putCopy Finished        = contain $ safePut (4 :: Int)
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return Unassigned
                           1 -> return Assigned
                           2 -> return BeingCalculated
                           3 -> return BeingTested
                           4 -> return Finished 

$(deriveSafeCopy 0 'base ''JobDetail)
$(deriveSafeCopy 0 'base ''JobInfo)

newtype JobInfoQueue = JobInfoQueue (Seq JobInfo)

$(deriveSafeCopy 0 'base ''JobInfoQueue)

addJob :: JobInfo -> Update JobInfoQueue ()
addJob job = do JobInfoQueue jq <- get 
                put $ JobInfoQueue (jq |> job) 

queryFirstJob :: Query JobInfoQueue (Maybe JobInfo)
queryFirstJob = do JobInfoQueue jq <- ask 
                   if length jq == 0 
                     then return Nothing
                     else return (Just (index jq 0))

$(makeAcidic ''JobInfoQueue ['addJob, 'queryFirstJob])