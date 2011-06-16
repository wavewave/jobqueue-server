{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module HEP.Automation.JobQueue.Test where

import Data.Acid 

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import System.Environment
import System.IO
import Data.SafeCopy 

import Data.Typeable

import qualified Data.Sequence as Seq

import HEP.Automation.JobQueue.Type
import HEP.Automation.JobQueue.Access 

test :: IO () 
test = do let j = JobDescription { 
                    jobdesc_jobtype = EventGeneration, 
                    jobdesc_pbsbatch = NotPBSBatchable, 
                    jobdesc_model = Model "sm"
                  }

          acid <- openAcidState (JobQueue Seq.empty) 
          update acid (InsertJob j)
          putStrLn "Done"
