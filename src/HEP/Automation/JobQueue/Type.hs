{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
----------------------------------------------------
--
-- Module       : HEP.Automation.JobQueue.Type
-- Copyright    : Ian-Woo Kim
-- License      : BSD3
-- 
-- Maintainer   : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability    : Experimental
-- Portability  : unknown 
-- 
-- Types for the job queue 
--
----------------------------------------------------

module HEP.Automation.JobQueue.Type ( 
  -- * Datatypes
  JobDescription(..), 
  JobType(..), 
  PBSBatchable(..), 
  Model(..), 
  JobStatus(..), 
  Installed(..), 
  ClientConfiguration(..)  
) where

data JobDescription = JobDescription {
  jobdesc_jobtype :: JobType, 
  jobdesc_pbsbatch :: PBSBatchable,
  jobdesc_model :: Model 
}

data JobType = EventGeneration | MathematicaAnalysis

data PBSBatchable = NotPBSBatchable | PBSBatchable

data Model = Model String 

data JobStatus = Unassigend 
               | Assigned 
               | BeingCalculated
               | BeingTested
               | Finished

data Installed = NotInstalled | Installed

data ClientConfiguration = ClientConfiguration {
  cliconf_mathematica :: Installed, 
  cliconf_pbs         :: Installed, 
  cliconf_models      :: [Model], 
  cliconf_madgraph    :: Installed
}