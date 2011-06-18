{-# LANGUAGE OverloadedStrings #-}

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

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.SetupType 

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
  fromAeson :: Value -> a


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

instance ToAeson MatchType where
  toAeson NoMatch = String "NoMatch"
  toAeson MLM = String "MLM"

instance ToAeson RGRunType where
  toAeson Fixed = String "Fixed"
  toAeson Auto  = String "Auto"

instance ToAeson CutType where
  toAeson NoCut  = String "NoCut"
  toAeson DefCut = String "DefCut"
  toAeson KCut   = String "KCut"

instance ToAeson PYTHIAType where
  toAeson NoPYTHIA = String "NoPYTHIA"
  toAeson RunPYTHIA = String "RunPYTHIA"

instance ToAeson UserCutSet where
  toAeson NoUserCutDef = Object (M.singleton "IsUserCutDefined" (String "NoUserCutDef"))
  toAeson (UserCutDef uc) = Object (M.fromList
                                     [ ("IsUserCutDefined", (String "UserCutDef"))
                                     , ("CutDetail", toAeson uc) ] )

instance ToAeson UserCut where
  toAeson (UserCut met etacutlep etcutlep etacutjet etcutjet) 
    = Array . V.fromList . map (Number . D) $ [met,etacutlep,etcutlep,etacutjet,etcutjet]

instance ToAeson PGSType where
  toAeson NoPGS  = String "NoPGS"
  toAeson RunPGS = String "RunPGS"
  toAeson RunPGSNoTau = String "RunPGSNoTau"

instance ToAeson MadGraphVersion where
  toAeson MadGraph4 = String "MadGraph4"
  toAeson MadGraph5 = String "MadGraph5"

instance (Model a) => ToAeson (ModelParam a) where
  toAeson = atomize . briefParamShow 


instance (Model a) => ToAeson (ProcessSetup a) where
  toAeson p = Object 
              $ M.fromList 
                    [ ("mversion"    , toAeson . mversion $ p)
                    , ("model"       , atomizeStr . modelName . model $ p)
                    , ("process"     , atomizeStr . process $ p)
                    , ("processBrief", atomizeStr . processBrief $ p)
                    , ("workname"    , atomizeStr . workname $ p) ]

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

instance ToAeson EventSet where
  toAeson (EventSet p r) = Object 
                 $ M.fromList 
                       [ ( "psetup" , toAeson p) 
                       , ( "rsetup" , toAeson r) ] 





