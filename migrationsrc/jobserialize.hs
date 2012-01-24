{-# LANGUAGE TypeOperators, TemplateHaskell, TypeFamilies, TypeSynonymInstances, OverlappingInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, EmptyDataDecls #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import HEP.Automation.JobQueue.JobQueue 
import HEP.Automation.JobQueue.JobType  
import HEP.Automation.JobQueue.JobJson

import Data.Acid
import qualified Data.IntMap as M

import Data.Aeson.Types
import Data.Aeson.Parser

{-
import Generics.Regular
import Generics.Regular.TH
import Generics.Regular.XmlPickler
import Text.XML.HXT.Arrow.Pickle

$(deriveAll ''EventSet "PFEventSet")
type instance PF EventSet = PFEventSet 

$(deriveAll ''JobPriority "PFJobPriority")
type instance PF JobPriority = PFJobPriority 

$(deriveAll ''JobDetail "PFJobDetail")
type instance PF JobDetail = PFJobDetail 

$(deriveAll ''JobStatus "PFJobStatus")
type instance PF JobStatus = PFJobStatus 

$(deriveAll ''JobInfo "PFJobInfo")
type instance PF JobInfo = PFJobInfo 

instance XmlPickler EventSet where
  xpickle = gxpickle

instance XmlPickler JobPriority where 
  xpickle = gxpickle 

instance XmlPickler JobDetail where
  xpickle = gxpickle

instance XmlPickler JobStatus where
  xpickle = gxpickle 

instance XmlPickler JobInfo where 
  xpickle = gxpickle
-}

import Data.Aeson.Encode
import Blaze.ByteString.Builder

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do 
  putStrLn "job migration."
  acid <- openAcidState (JobInfoQueue 0 M.empty) -- From "state/HEP.Automation.JobQueue.JobQueue.JobInfoQueue" (JobInfoQueue 0 M.empty) 
  (i,jinfos) <- query acid QueryAll  
  let bstr = (toLazyByteString . fromValue . toAeson)  $  jinfos

  L.writeFile "currentcontent.json" bstr 