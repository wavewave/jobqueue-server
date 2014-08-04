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

instance (Data a) => FromJSON a where
  parseJSON v = let r = G.fromJSON v 
                in case r of 
                     Success a -> return a 
                     Error _str -> fail $ (show . typeOf) (undefined :: a) ++ " is not parsed"

instance ToJSON MachineType where
  toJSON TeVatron = object [ "Type" .= String "TeVatron" ]
  toJSON (LHC7 detector) = object [ "Type" .= String "LHC7"
                                  , "Detector" .= G.toJSON detector ]
  toJSON (LHC14 detector)  = object [ "Type" .= String "LHC14" 
                                    , "Detector" .= G.toJSON detector ]
  toJSON (Parton energy detector) = object [ "Type" .= String "Parton"
                                           , "Energy" .= toJSON energy 
                                           , "Detector" .= G.toJSON detector ] 
  toJSON (PolParton energy ipol detector) = 
    let p1 = (rhpol_percent . particle1pol) ipol 
        p2 = (rhpol_percent . particle2pol) ipol 
    in  object [ "Type" .= String "Parton"
               , "Energy" .= toJSON energy
               , "Detector" .= G.toJSON detector
               , "InitPol1" .= toJSON p1 
               , "InitPol2" .= toJSON p2 ] 

instance FromJSON MachineType where
  parseJSON (Object m) = do
    t <- elookup "Type" m
    case t of 
      String "TeVatron" -> return TeVatron 
      String "LHC7"     -> LHC7 <$> lookupfunc "Detector" m 
      String "LHC14"    -> LHC14 <$> lookupfunc "Detector" m
      String "Parton"   -> do 
        Parton <$> lookupfunc "Energy" m 
               <*> lookupfunc "Detector" m
      String "PolParton"   -> do 
        energy   <- lookupfunc "Energy" m 
        ipol1    <- lookupfunc "InitPol1" m 
        ipol2    <- lookupfunc "InitPol2" m
        detector <- lookupfunc "Detector" m
        return (PolParton energy 
                          (InitPolarization (RH ipol1) (RH ipol2)) 
                          detector)
      _ -> fail "MachineType not parsed"
  parseJSON _ = fail "MachineType not parsed"

instance ToJSON MatchType where
  toJSON NoMatch = "NoMatch"
  toJSON MLM = "MLM"

instance FromJSON MatchType where
  parseJSON (String "NoMatch") = return NoMatch
  parseJSON (String "MLM") = return MLM 
  parseJSON _ = fail "MatchType Not Parsed"

instance ToJSON RGRunType where
  toJSON Fixed = "Fixed"
  toJSON Auto  = "Auto"

instance FromJSON RGRunType where
  parseJSON (String "Fixed") = return Fixed
  parseJSON (String "Auto") = return Auto
  parseJSON _ = fail "RGRunType Not Parsed"

instance ToJSON CutType where
  toJSON NoCut  = "NoCut"
  toJSON DefCut = "DefCut"
  toJSON KCut   = "KCut"

instance FromJSON CutType where
  parseJSON (String "NoCut") = return NoCut
  parseJSON (String "DefCut") = return DefCut 
  parseJSON (String "KCut") = return KCut 
  parseJSON _ = fail "CutType Not Parsed"

instance ToJSON PYTHIAType where
  toJSON NoPYTHIA =  "NoPYTHIA"
  toJSON RunPYTHIA = "RunPYTHIA"

instance FromJSON PYTHIAType where
  parseJSON (String "NoPYTHIA") = return NoPYTHIA
  parseJSON (String "RunPYTHIA") = return RunPYTHIA
  parseJSON _ = fail "PYTHIAType not parsed"

instance ToJSON PGSType where
  toJSON NoPGS            = object [ "Type" .= String "NoPGS" ]
  toJSON (RunPGS algotau) = object [ "Type" .= String "RunPGS"
                                   , "JetAlgoAndTau" .= toJSON algotau ]

instance FromJSON PGSType where
  parseJSON (Object m) = do 
    t <- elookup "Type" m
    case t of
      String "NoPGS" -> return NoPGS
      String "RunPGS" -> RunPGS <$> lookupfunc "JetAlgoAndTau" m
      _ -> fail "PGSType not parsed"
  parseJSON _ = fail "PGSType not parsed"




instance ToJSON PGSTau where
  toJSON NoTau   = "NoTau"
  toJSON WithTau = "WithTau"
   
instance FromJSON PGSTau where
  parseJSON (String "NoTau")   = return NoTau
  parseJSON (String "WithTau") = return WithTau

instance ToJSON PGSJetAlgorithm where
  toJSON (Cone sz)      = object [ "Type" .= String "Cone" 
                                 , "Size" .= toJSON sz ]
  toJSON (KTJet sz)     = object [ "Type" .= String "KTJet"
                                 , "Size" .= toJSON sz ]
  toJSON (AntiKTJet sz) = object [ "Type" .= String "AntiKTJet"
                                 , "Size" .= toJSON sz ]

instance FromJSON PGSJetAlgorithm where
  parseJSON (Object m) = do 
    t <- elookup "Type" m
    case t of
      String "Cone"      -> Cone      <$> lookupfunc "Size" m
      String "KTJet"     -> KTJet     <$> lookupfunc "Size" m 
      String "AntiKTJet" -> AntiKTJet <$> lookupfunc "Size" m
      _ -> fail "PGSJetAlgorithm not parsed"
  parseJSON _ = fail "PGSJetAlgorithm not parsed"



instance ToJSON MadGraphVersion where
  toJSON MadGraph4 = "MadGraph4"
  toJSON MadGraph5 = "MadGraph5"

instance FromJSON MadGraphVersion where
  parseJSON (String "MadGraph4") = return MadGraph4
  parseJSON (String "MadGraph5") = return MadGraph5
  parseJSON _ = fail "MadGraphVersion not parsed"

instance (Model a) => ToJSON (ModelParam a) where
  toJSON p = let str = briefParamShow p  
              in  String (pack str) 

instance (Model a) => FromJSON (ModelParam a) where
  parseJSON (String str) = return . interpreteParam . unpack $ str
  parseJSON _ = fail "ModelParam not parsed"

-- | parsing model from JSON object
modelFromJSON :: (Model a) => Value -> Parser a 
modelFromJSON (String str) = maybe (fail "modelFromJSON failed") return $ modelFromString . unpack $ str
modelFromJSON _ = fail "modelFromJSON failed"

instance (Model a) => ToJSON (ProcessSetup a) where
  toJSON p = object [ "model" .= ( atomizeStr . modelName . model $ p )
                    , "process" .= toJSON (process p)
                    , "processBrief" .= ( atomizeStr . processBrief $ p )
                    , "workname" .= ( atomizeStr . workname $ p) 
                    , "hashSalt" .= toJSON (hashSalt p)
                    ]

instance (Model a) => FromJSON (ProcessSetup a) where
  parseJSON (Object m) = PS <$> (elookup "model" m >>= modelFromJSON)
                            <*> lookupfunc "process" m
                            <*> lookupfunc "processBrief" m
                            <*> lookupfunc "workname" m
                            <*> lookupfunc "hashSalt" m
  parseJSON _ = fail "ProcessSetup not parsed"
 
instance ToJSON MGProcess where
  toJSON MGProc {..} = object [ "definelines" .= toJSON mgp_definelines 
                              , "processes"   .= toJSON mgp_processes
                              ] 

instance FromJSON MGProcess where
  parseJSON (Object m) = MGProc <$> lookupfunc "definelines" m
                                <*> lookupfunc "processes" m
  parseJSON _ = fail "MGProcess not parsed"

instance ToJSON HashSalt where
  toJSON s = toJSON (unHashSalt s)

instance FromJSON HashSalt where
  parseJSON v = HashSalt <$> parseJSON v

instance ToJSON RunSetup where
  toJSON p = object [ "numevent"  .= (toJSON . numevent $ p)
                    , "machine"   .= (toJSON . machine $ p)
                    , "rgrun"     .= (toJSON . rgrun $ p)
                    , "rgscale"   .= (toJSON . rgscale $ p)
                    , "match"     .= (toJSON . match $ p)
                    , "cut"       .= (toJSON . cut $ p) 
                    , "pythia"    .= (toJSON . pythia $ p)
                    , "lhesanitizer" .= (G.toJSON . lhesanitizer $ p)
                    , "pgs"       .= (toJSON . pgs $ p) 
                    , "hep"       .= (G.toJSON . uploadhep $ p)
                    , "setnum"    .= (toJSON . setnum $ p) ] 

instance FromJSON RunSetup where
  parseJSON (Object m) =   RS <$> lookupfunc "numevent" m
                              <*> lookupfunc "machine" m 
                              <*> lookupfunc "rgrun" m
                              <*> lookupfunc "rgscale" m 
                              <*> lookupfunc "match" m
                              <*> lookupfunc "cut" m     
                              <*> lookupfunc "pythia" m
                              <*> lookupfunc "lhesanitizer" m
                              <*> lookupfunc "pgs" m    
                              <*> lookupfunc "hep" m     
                              <*> lookupfunc "setnum" m
  parseJSON _ = fail "RunSetup not parsed"

instance ToJSON EventSet where
  toJSON (EventSet p param r) = object [ "psetup" .= toJSON p
                                       , "param"  .= toJSON param
                                       , "rsetup" .= toJSON r ] 

instance FromJSON EventSet where
  parseJSON (Object m) = do 
    psobj <- elookup "psetup" m 
    case psobj of 
      Object ps -> do 
        mdl <- elookup "model" ps
        case mdl of 
          String str -> do 
            modelbox <- maybe (fail "model in EventSet failed") return $ modelParse (unpack str) 
            mkEventSet modelbox   
          _ -> fail "model in EventSet failed"
      _ -> fail "psetup in EventSet failed"
    where mkEventSet :: ModelBox -> Parser EventSet
          mkEventSet (ModelBox mdl) = 
               EventSet <$> getPSetup mdl <*> getParam mdl <*> getRSetup
          getPSetup :: (Model a) => a -> Parser (ProcessSetup a) 
          getPSetup _mdl = lookupfunc "psetup" m
          getParam :: (Model a) => a -> Parser (ModelParam a)
          getParam _mdl = lookupfunc "param" m
          getRSetup :: Parser RunSetup
          getRSetup = lookupfunc "rsetup" m
  parseJSON _ = fail "EventSet not parsed"

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

instance ToJSON WebDAVRemoteDir where
  toJSON (WebDAVRemoteDir rdir) = toJSON rdir 

instance FromJSON WebDAVRemoteDir where
  parseJSON v = WebDAVRemoteDir <$> parseJSON v

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

