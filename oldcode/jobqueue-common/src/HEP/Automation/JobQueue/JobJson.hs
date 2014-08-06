{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.JobQueue.JobJson 
-- Copyright   : (c) 2011, 2012, 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Types for a event gen job
--
----------------------------------------------------

module HEP.Automation.JobQueue.JobJson where

import Control.Applicative

import HEP.Automation.MadGraph.Card
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.ModelParser
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType 
import HEP.Automation.MadGraph.Type
import HEP.Parser.LHE.Sanitizer.Type

import HEP.Storage.WebDAV.Type

import Data.Text hiding (map)
import Data.Aeson.Types hiding (parse)
import qualified Data.Vector as V
import qualified  Data.HashMap.Strict as M

import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config 

import Data.Data
import qualified Data.Aeson.Generic as G

-- | 
atomize :: (Show a) => a -> Value 
atomize = atomizeStr . show 

-- | 
atomizeStr :: String -> Value
atomizeStr = String . pack

-- |
elookup :: Text -> M.HashMap Text Value -> Parser Value
elookup k m = maybe (fail (unpack k ++ " not parsed")) 
                    return 
                    (M.lookup k m)

-- | 
lookupfunc :: (FromJSON a) => Text -> M.HashMap Text Value -> Parser a
lookupfunc k m = elookup k m >>= parseJSON 

{- 
instance (Data a) => FromJSON a where
  parseJSON v = let r = G.fromJSON v 
                in case r of 
                     Success a -> return a 
                     Error _str -> fail $ (show . typeOf) (undefined :: a) ++ " is not parsed"
-}

instance ToJSON JobDetail where
  toJSON (EventGen evset rdir) = object [ "JobType" .= String "EventGen" 
                                        , "evset"   .= toJSON evset 
                                        , "rdir"    .= toJSON rdir ]
  toJSON (MathAnal mathanal evset rdir) = 
    object [ "JobType"  .= String "MathAnal" 
           , "mathanal" .= toJSON mathanal
           , "evset"    .= toJSON evset
           , "rdir"     .= toJSON rdir ]

instance FromJSON JobDetail where
  parseJSON (Object m) = do
    t <- elookup "JobType" m 
    case t of 
      String "EventGen" -> EventGen <$> (elookup "evset" m >>= parseJSON) 
                                    <*> (elookup "rdir" m >>= parseJSON)
      String "MathAnal" -> MathAnal <$> lookupfunc "mathanal" m 
                                    <*> (elookup "evset" m >>= parseJSON)
                                    <*> (elookup "rdir" m >>= parseJSON)
      _ -> fail "JobType in JobDetail failed" 
  parseJSON _ = fail "JobDetail not parsed"

instance ToJSON JobInfo where
  toJSON i = object [ "id"         .= (toJSON . jobinfo_id $ i)
                    , "detail"     .= (toJSON . jobinfo_detail $ i)
                    , "status"     .= (G.toJSON . jobinfo_status $ i) 
                    , "priority"   .= (G.toJSON . jobinfo_priority $ i) 
                    , "dependency" .= (toJSON . jobinfo_dependency $ i) ]

instance FromJSON JobInfo where
  parseJSON (Object m) = 
    JobInfo <$> lookupfunc "id" m
            <*> lookupfunc "detail" m
            <*> lookupfunc "status" m
            <*> lookupfunc "priority" m
            <*> lookupfunc "dependency" m 
  parseJSON _ = fail "JobInfo not parsed"
    

instance ToJSON ClientConfiguration where
  toJSON (ClientConfiguration computer math pbs montecarlo datasetdir) = 
      object [ "computer"    .= toJSON computer 
             , "mathematica" .= toJSON math
             , "pbs"         .= toJSON pbs   
             , "montecarlo"  .= toJSON montecarlo 
             , "datasetDir"  .= toJSON datasetdir ] 

instance FromJSON ClientConfiguration where
  parseJSON (Object m) = 
    ClientConfiguration 
      <$> lookupfunc "computer" m
      <*> lookupfunc "mathematica" m 
      <*> lookupfunc "pbs" m
      <*> lookupfunc "montecarlo" m
      <*> lookupfunc "datasetDir" m
  parseJSON _ = fail "ClientConfiguration not parsed"


instance ToJSON URLtype where
  toJSON (LocalURL fp)  = object [ "Type" .= String "LocalURL" 
                                 , "url"  .= toJSON fp
                                 ] 
  toJSON (GlobalURL url) = object [ "Type" .= String "GlobalURL"
                                  , "url"  .= toJSON url ]

instance FromJSON URLtype where
  parseJSON (Object m) = do 
    t <- elookup "Type" m
    case t of
      String "LocalURL"  -> LocalURL  <$> lookupfunc "url" m  
      String "GlobalURL" -> GlobalURL <$> lookupfunc "url" m
      _ -> fail "URLtype not parsed"
  parseJSON _ = fail "URLtype not parsed"

