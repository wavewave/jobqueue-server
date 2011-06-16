{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module HEP.Automation.JobQueue.Access where

import Data.Acid 

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import System.Environment
import System.IO
import Data.SafeCopy 

import Data.Typeable

import qualified Data.Map as Map 

import HEP.Automation.JobQueue.Type

import Data.Sequence 

data JobQueue = JobQueue (Seq JobDescription) 
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''JobQueue)

insertJob :: JobDescription -> Update JobQueue () 
insertJob j = do
  JobQueue s <- get 
  put (JobQueue ( s |> j )) 


lookupJob :: JobDescription -> Query JobQueue (Maybe JobDescription) 
lookupJob = undefined 

$(makeAcidic ''JobQueue ['insertJob])

{-
type Key = String 
type Value = String 

data KeyValue = KeyValue !(Map.Map Key Value)
  deriving (Typeable) 

$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: Key -> Value -> Update KeyValue () 
insertKey key value 
  = do KeyValue m <- get 
       put (KeyValue (Map.insert key value m))

lookupKey :: Key -> Query KeyValue (Maybe Value) 
lookupKey key 
  = do KeyValue m <- ask 
       return (Map.lookup key m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])
-}