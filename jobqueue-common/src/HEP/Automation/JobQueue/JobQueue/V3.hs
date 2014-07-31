v v v v v v v
=============
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

module HEP.Automation.JobQueue.JobQueue.V3 where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Typeable

import Data.Acid


import HEP.Automation.JobQueue.JobType
import HEP.Storage.WebDAV.Type

import Prelude hiding (length)
-- import Data.Sequence
import Data.SafeCopy

import qualified Data.IntMap as M

type JobNumber = Int

data JobInfo = JobInfo {
  jobinfo_id     :: JobNumber, 
  jobinfo_detail :: JobDetail, 
  jobinfo_status :: JobStatus
} deriving (Typeable, Show) 

data JobDetail = EventGen { jobdetail_evset :: EventSet, 
                            jobdetail_remotedir :: WebDAVRemoteDir
                          }  
               | MathAnal { jobdetail_evset :: EventSet, 
                            jobdetail_remotedir :: WebDAVRemoteDir
                          } 
               deriving Show

data JobStatus = Unassigned 
               | Assigned 
               | BeingCalculated
               | BeingTested
               | Finished
               deriving (Show, Eq) 


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

data JobInfoQueue = JobInfoQueue { 
                      jobinfoqueue_lastid :: Int,
                      jobinfoqueue_map    :: M.IntMap JobInfo 
                    }
                  deriving (Typeable, Show)

$(deriveSafeCopy 0 'base ''JobInfoQueue) 

addJob :: JobDetail -> Update JobInfoQueue () 
addJob j = do JobInfoQueue lastid m <- get
              let newjob = JobInfo (lastid+1) j Unassigned
              put $ JobInfoQueue (lastid+1) (M.insert (lastid+1) newjob m)  

queryAll :: Query JobInfoQueue (Int, [JobInfo]) 
queryAll = do JobInfoQueue lastid m <- ask 
              return (lastid, M.elems m)

queryJob :: Int -> Query JobInfoQueue (Maybe JobInfo) 
queryJob k = do JobInfoQueue _ m <- ask
                return (M.lookup k m)

updateJob :: Int -> JobInfo -> Update JobInfoQueue (Maybe JobInfo)
updateJob k jinfo 
    | k /= jobinfo_id jinfo = return Nothing
    | k == jobinfo_id jinfo = 
        do JobInfoQueue l m <- get 
           let f k a = Just jinfo 
           let (r,m') = M.updateLookupWithKey f k m
           put (JobInfoQueue l m')
           return r 

deleteJob :: Int -> Update JobInfoQueue ()
deleteJob k = do 
  JobInfoQueue l m <- get
  let m' = M.delete k m
  put (JobInfoQueue l m')

 
$(makeAcidic ''JobInfoQueue ['addJob, 'queryAll, 'queryJob, 'updateJob, 'deleteJob]) 



*************
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

module HEP.Automation.JobQueue.JobQueue.V3 where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Typeable

import Data.Acid


import HEP.Automation.JobQueue.JobType.V3
import HEP.Storage.WebDAV.Type

import Prelude hiding (length)
-- import Data.Sequence
import Data.SafeCopy

import qualified Data.IntMap as M

type JobNumber = Int

data JobInfo = JobInfo {
  jobinfo_id     :: JobNumber, 
  jobinfo_detail :: JobDetail, 
  jobinfo_status :: JobStatus
} deriving (Typeable, Show) 

data JobDetail = EventGen { jobdetail_evset :: EventSet, 
                            jobdetail_remotedir :: WebDAVRemoteDir
                          }  
               | MathAnal { jobdetail_evset :: EventSet, 
                            jobdetail_remotedir :: WebDAVRemoteDir
                          } 
               deriving Show

data JobStatus = Unassigned 
               | Assigned 
               | BeingCalculated
               | BeingTested
               | Finished
               deriving (Show, Eq) 


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

data JobInfoQueue = JobInfoQueue { 
                      jobinfoqueue_lastid :: Int,
                      jobinfoqueue_map    :: M.IntMap JobInfo 
                    }
                  deriving (Typeable, Show)

$(deriveSafeCopy 0 'base ''JobInfoQueue) 

addJob :: JobDetail -> Update JobInfoQueue () 
addJob j = do JobInfoQueue lastid m <- get
              let newjob = JobInfo (lastid+1) j Unassigned
              put $ JobInfoQueue (lastid+1) (M.insert (lastid+1) newjob m)  

queryAll :: Query JobInfoQueue (Int, [JobInfo]) 
queryAll = do JobInfoQueue lastid m <- ask 
              return (lastid, M.elems m)

queryJob :: Int -> Query JobInfoQueue (Maybe JobInfo) 
queryJob k = do JobInfoQueue _ m <- ask
                return (M.lookup k m)

updateJob :: Int -> JobInfo -> Update JobInfoQueue (Maybe JobInfo)
updateJob k jinfo 
    | k /= jobinfo_id jinfo = return Nothing
    | k == jobinfo_id jinfo = 
        do JobInfoQueue l m <- get 
           let f k a = Just jinfo 
           let (r,m') = M.updateLookupWithKey f k m
           put (JobInfoQueue l m')
           return r 

deleteJob :: Int -> Update JobInfoQueue ()
deleteJob k = do 
  JobInfoQueue l m <- get
  let m' = M.delete k m
  put (JobInfoQueue l m')

 
$(makeAcidic ''JobInfoQueue ['addJob, 'queryAll, 'queryJob, 'updateJob, 'deleteJob]) 



^ ^ ^ ^ ^ ^ ^
