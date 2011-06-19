{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

----------------------------------------------------
--
-- Module       : HEP.Automation.JobQueue.JobJson
-- Copyright    : Ian-Woo Kim
-- License      : BSD3
-- 
-- Maintainer   : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability    : Experimental
-- Portability  : unknown 
-- 
-- Types for a event gen job
--
----------------------------------------------------

module HEP.Automation.JobQueue.JobJson where

import Control.Applicative

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.SetupType 

import HEP.Automation.MadGraph.Model.AxiGluon

import Data.Text hiding (map)
import Data.Attoparsec.Number 
import Data.Aeson.Types 

import qualified Data.Vector as V
import qualified Data.Map as M



import HEP.Automation.JobQueue.JobType

--import Control.Applicative

class ToAeson a where 
  toAeson :: a -> Value

class FromAeson a where
  fromAeson :: Value -> Maybe a


atomize :: (Show a) => a -> Value 
atomize = atomizeStr . show 

atomizeStr :: String -> Value
atomizeStr = String . pack

instance ToAeson MachineType where
  toAeson TeVatron = Object (M.singleton "Type" (String "TeVatron"))
  toAeson LHC7     = Object (M.singleton "Type" (String "LHC7"))
  toAeson LHC14    = Object (M.singleton "Type" (String "LHC14"))
  toAeson (Parton energy) = Object (M.fromList 
                                         [ ("Type",String "Parton")
                                         , ("Energy",Number (D energy)) ]) 

instance FromAeson MachineType where
  fromAeson (Object m) = do t <- M.lookup "Type" m
                            case t of 
                              String "TeVatron" -> return TeVatron 
                              String "LHC7"     -> return LHC7
                              String "LHC14"    -> return LHC14
                              String "Parton"   -> do 
                                e <- M.lookup "Energy" m
                                case e of 
                                  Number (D energy) -> return (Parton energy)
                                  _ -> Nothing
                              _ -> Nothing
  fromAeson _ = Nothing 


instance ToAeson MatchType where
  toAeson NoMatch = String "NoMatch"
  toAeson MLM = String "MLM"


instance FromAeson MatchType where
  fromAeson (String "NoMatch") = Just NoMatch
  fromAeson (String "MLM") = Just MLM 
  fromAeson _ = Nothing

instance ToAeson RGRunType where
  toAeson Fixed = String "Fixed"
  toAeson Auto  = String "Auto"

instance FromAeson RGRunType where
  fromAeson (String "Fixed") = Just Fixed
  fromAeson (String "Auto") = Just Auto
  fromAeson _ = Nothing

instance ToAeson CutType where
  toAeson NoCut  = String "NoCut"
  toAeson DefCut = String "DefCut"
  toAeson KCut   = String "KCut"

instance FromAeson CutType where
  fromAeson (String "NoCut") = Just NoCut
  fromAeson (String "DefCut") = Just DefCut 
  fromAeson (String "KCut") = Just KCut 
  fromAeson _ = Nothing

instance ToAeson PYTHIAType where
  toAeson NoPYTHIA = String "NoPYTHIA"
  toAeson RunPYTHIA = String "RunPYTHIA"

instance FromAeson PYTHIAType where
  fromAeson (String "NoPYTHIA") = Just NoPYTHIA
  fromAeson (String "RunPYTHIA") = Just RunPYTHIA
  fromAeson _ = Nothing

instance ToAeson UserCutSet where
  toAeson NoUserCutDef = Object (M.singleton "IsUserCutDefined" (String "NoUserCutDef"))
  toAeson (UserCutDef uc) = Object (M.fromList
                                     [ ("IsUserCutDefined", (String "UserCutDef"))
                                     , ("CutDetail", toAeson uc) ] )

instance FromAeson UserCutSet where
  fromAeson (Object m) = do t <- M.lookup "IsUserCutDefined" m 
                            case t of 
                              String "NoUserCutDef" -> return NoUserCutDef
                              String "UserCutDef" -> do 
                                d <- M.lookup "CutDetail" m
                                fromAeson d
                              _ -> Nothing 
  fromAeson _ = Nothing


instance FromAeson Double where
  fromAeson (Number (D val)) = Just val 
  fromAeson _ = Nothing

instance ToAeson UserCut where
  toAeson (UserCut met etacutlep etcutlep etacutjet etcutjet) 
    = Array . V.fromList . map (Number . D) $ [met,etacutlep,etcutlep,etacutjet,etcutjet]

instance FromAeson UserCut where
  fromAeson (Array v) | V.length v == 5 = do 
    met       <- fromAeson (v V.! 0)
    etacutlep <- fromAeson (v V.! 1)
    etcutlep  <- fromAeson (v V.! 2)
    etacutjet <- fromAeson (v V.! 3)
    etcutjet  <- fromAeson (v V.! 4) 
    return (UserCut met etacutlep etcutlep etacutjet etcutjet)
                      | otherwise = Nothing
  fromAeson _ = Nothing 
 

instance ToAeson PGSType where
  toAeson NoPGS  = String "NoPGS"
  toAeson RunPGS = String "RunPGS"
  toAeson RunPGSNoTau = String "RunPGSNoTau"

instance FromAeson PGSType where
  fromAeson (String "NoPGS") = Just NoPGS
  fromAeson (String "RunPGS") = Just RunPGS
  fromAeson (String "RunPGSNoTau") = Just RunPGSNoTau
  fromAeson _ = Nothing


instance ToAeson MadGraphVersion where
  toAeson MadGraph4 = String "MadGraph4"
  toAeson MadGraph5 = String "MadGraph5"

instance FromAeson MadGraphVersion where
  fromAeson (String "MadGraph4") = Just MadGraph4
  fromAeson (String "MadGraph5") = Just MadGraph5
  fromAeson _ = Nothing

instance (Model a) => ToAeson (ModelParam a) where
  toAeson = atomize . briefParamShow 

instance (Model a) => FromAeson (ModelParam a) where
  fromAeson (String value) = Just . interpreteParam . unpack $ value
  fromAeson _ = Nothing 

--instance (Model a) => FromAeson a where
-- fromAeson (String str) = modelFromString . unpack $ str  

modelFromAeson :: (Model a) => Value -> Maybe a 
modelFromAeson (String str) = modelFromString . unpack $ str
modelFromAeson _ = Nothing

instance FromAeson String where
  fromAeson (String str) = Just . unpack $ str  
  fromAeson _ = Nothing 

instance (Model a) => ToAeson (ProcessSetup a) where
  toAeson p = Object 
              $ M.fromList 
                    [ ("mversion"    , toAeson . mversion $ p)
                    , ("model"       , atomizeStr . modelName . model $ p)
                    , ("process"     , atomizeStr . process $ p)
                    , ("processBrief", atomizeStr . processBrief $ p)
                    , ("workname"    , atomizeStr . workname $ p) ]

instance (Model a) => FromAeson (ProcessSetup a) where
  fromAeson (Object m) =  
    PS <$> lookupfunc "mversion" <*> (M.lookup "model" m >>= modelFromAeson)
       <*> lookupfunc "process"  <*> lookupfunc "processBrief" 
       <*> lookupfunc "workname" 
    where lookupfunc str = M.lookup str m >>= fromAeson  
  fromAeson _ = Nothing 
 

instance FromAeson Int where
  fromAeson (Number (I val)) = Just (fromIntegral val) 
  fromAeson _ = Nothing

instance (Model a) => ToAeson (RunSetup a) where
  toAeson p = Object 
              $ M.fromList 
                    [ ("param"    , toAeson . param $ p)
                    , ("numevent" , Number . I . fromIntegral . numevent $ p)
                    , ("machine"  , toAeson . machine $ p)
                    , ("rgrun"    , toAeson . rgrun $ p)
                    , ("rgscale"  , Number . D . rgscale $ p)
                    , ("match"    , toAeson . match $ p)
                    , ("cut"      , toAeson . cut $ p) 
                    , ("pythia"   , toAeson . pythia $ p)
                    , ("usercut"  , toAeson . usercut $ p)
                    , ("pgs"      , toAeson . pgs $ p) 
                    , ("setnum"   , Number . I . fromIntegral . setnum $ p)] 

instance (Model a) => FromAeson (RunSetup a) where
  fromAeson (Object m) = 
    RS <$> lookupfunc "param"   <*> lookupfunc "numevent" 
       <*> lookupfunc "machine" <*> lookupfunc "rgrun" 
       <*> lookupfunc "rgscale" <*> lookupfunc "match"
       <*> lookupfunc "cut"     <*> lookupfunc "pythia"  
       <*> lookupfunc "usercut" <*> lookupfunc "pgs"     
       <*> lookupfunc "setnum"
    where lookupfunc str = M.lookup str m >>= fromAeson  
  fromAeson _ = Nothing

instance ToAeson EventSet where
  toAeson (EventSet p r) = Object 
                 $ M.fromList 
                       [ ( "psetup" , toAeson p) 
                       , ( "rsetup" , toAeson r) ] 

instance FromAeson EventSet where
  fromAeson (Object m) = do 
    psobj <- M.lookup "psetup" m 
    case psobj of 
      Object ps -> do 
        mdl <- M.lookup "model" ps
        case mdl of 
          String "AxiGluon" ->  
            EventSet <$> (lookupfunc "psetup" :: Maybe (ProcessSetup AxiGluon)) 
                     <*> (lookupfunc "rsetup" :: Maybe (RunSetup AxiGluon))
          String "DummyModel" -> 
            EventSet <$> (lookupfunc "psetup" :: Maybe (ProcessSetup DummyModel))
                     <*> (lookupfunc "rsetup" :: Maybe (RunSetup DummyModel))
          _ -> Nothing 
      _ -> Nothing
    where lookupfunc str = M.lookup str m >>= fromAeson
  fromAeson _ = Nothing 

