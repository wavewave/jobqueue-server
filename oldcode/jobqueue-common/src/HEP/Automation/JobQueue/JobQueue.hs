{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.JobQueue.JobQueue 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
----------------------------------------------------

module HEP.Automation.JobQueue.JobQueue where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.Data
import qualified Data.IntMap as M
import Data.SafeCopy
import Data.Typeable
-- 
import HEP.Automation.EventGeneration.Type
import HEP.Storage.WebDAV.Type
-- 
import HEP.Automation.JobQueue.JobType
-- 
import Prelude hiding (length)

-- newtype JobNumber = JobNum { unJobNum :: Int}
type JobNumber = Int

data JobPriority = NonUrgent | Urgent   
                 deriving (Show,Eq,Ord,Typeable,Data)
       
data JobInfo = JobInfo {
  jobinfo_id     :: JobNumber, 
  jobinfo_detail :: JobDetail, 
  jobinfo_status :: JobStatus, 
  jobinfo_priority :: JobPriority, 
  jobinfo_dependency :: [JobNumber] 
} deriving (Show, Typeable) 

type ManyJobInfo = [ (Int, JobInfo) ] 

data JobDetail = EventGen { jobdetail_evset :: EventSet, 
                            jobdetail_remotedir :: WebDAVRemoteDir
                          }  
               | MathAnal { jobdetail_mathanal :: String, 
                            jobdetail_evset :: EventSet, 
                            jobdetail_remotedir :: WebDAVRemoteDir
                          } 
               deriving (Show, Typeable)

data JobStatus = Unassigned 
               | Assigned String 
               | BeingCalculated String 
               | BeingTested String 
               | Finished String 
               deriving (Show, Eq, Typeable, Data) 


instance SafeCopy JobPriority where
  putCopy NonUrgent = contain $ safePut ( 0 :: Int ) 
  putCopy Urgent    = contain $ safePut ( 1 :: Int ) 
  getCopy = contain $ do ( x :: Int ) <- safeGet  
                         case x of 
                           0 -> return NonUrgent 
                           1 -> return Urgent 

instance SafeCopy JobStatus where
  putCopy Unassigned            = contain $ safePut (0 :: Int)
  putCopy (Assigned        str) = contain $ do {safePut (1 :: Int); safePut str}
  putCopy (BeingCalculated str) = contain $ do {safePut (2 :: Int); safePut str}
  putCopy (BeingTested     str) = contain $ do {safePut (3 :: Int); safePut str}
  putCopy (Finished        str) = contain $ do {safePut (4 :: Int); safePut str}
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return Unassigned
                           1 -> Assigned <$> safeGet  
                           2 -> BeingCalculated <$> safeGet
                           3 -> BeingTested <$> safeGet
                           4 -> Finished <$> safeGet

$(deriveSafeCopy 0 'base ''JobDetail)
$(deriveSafeCopy 0 'base ''JobInfo)

data JobInfoQueue = JobInfoQueue { 
                      jobinfoqueue_lastid :: Int,
                      jobinfoqueue_map    :: M.IntMap JobInfo 
                    }
                  deriving (Typeable, Show)

$(deriveSafeCopy 0 'base ''JobInfoQueue) 

addJob :: JobDetail -> Update JobInfoQueue (Int,JobInfo) 
addJob j = addJobWithPriority j NonUrgent 


addJobWithPriority :: JobDetail -> JobPriority -> Update JobInfoQueue (Int,JobInfo) 
addJobWithPriority j p = do JobInfoQueue lastid m <- get
                            let newjob = JobInfo (lastid+1) j Unassigned p []
                            put $ JobInfoQueue (lastid+1) (M.insert (lastid+1) newjob m)  
                            return (lastid+1,newjob)

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

 
$(makeAcidic ''JobInfoQueue [ 'addJob, 'addJobWithPriority, 'queryAll
                            , 'queryJob, 'updateJob, 'deleteJob]) 



