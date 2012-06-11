{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances,
             UndecidableInstances, ScopedTypeVariables, ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.JobQueue.JobJson 
-- Copyright   : (c) 2011, 2012 Ian-Woo Kim
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

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.SetupType 

-- import HEP.Automation.MadGraph.Model.AxiGluon
import HEP.Automation.MadGraph.ModelParser

import HEP.Storage.WebDAV.Type

import Data.Text hiding (map)
-- import Data.Attoparsec 
-- import Data.Attoparsec.Number 
import Data.Aeson.Types hiding (parse)
import qualified Data.Vector as V
-- import qualified Data.Map as M
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

-- | 

instance (Data a) => FromJSON a where
  parseJSON v = let r = G.fromJSON v 
                in case r of 
                     Success a -> return a 
                     Error _str -> fail $ (show . typeOf) (undefined :: a) ++ " is not parsed"

-- | 

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

-- | 

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

-- |

instance ToJSON MatchType where
  toJSON NoMatch = "NoMatch"
  toJSON MLM = "MLM"

-- | 

instance FromJSON MatchType where
  parseJSON (String "NoMatch") = return NoMatch
  parseJSON (String "MLM") = return MLM 
  parseJSON _ = fail "MatchType Not Parsed"

-- | 

instance ToJSON RGRunType where
  toJSON Fixed = "Fixed"
  toJSON Auto  = "Auto"

-- | 

instance FromJSON RGRunType where
  parseJSON (String "Fixed") = return Fixed
  parseJSON (String "Auto") = return Auto
  parseJSON _ = fail "RGRunType Not Parsed"

-- | 

instance ToJSON CutType where
  toJSON NoCut  = "NoCut"
  toJSON DefCut = "DefCut"
  toJSON KCut   = "KCut"

-- | 

instance FromJSON CutType where
  parseJSON (String "NoCut") = return NoCut
  parseJSON (String "DefCut") = return DefCut 
  parseJSON (String "KCut") = return KCut 
  parseJSON _ = fail "CutType Not Parsed"

-- | 

instance ToJSON PYTHIAType where
  toJSON NoPYTHIA =  "NoPYTHIA"
  toJSON RunPYTHIA = "RunPYTHIA"

-- | 

instance FromJSON PYTHIAType where
  parseJSON (String "NoPYTHIA") = return NoPYTHIA
  parseJSON (String "RunPYTHIA") = return RunPYTHIA
  parseJSON _ = fail "PYTHIAType not parsed"

-- | 

instance ToJSON UserCutSet where
  toJSON NoUserCutDef = object [ "IsUserCutDefined" .= String "NoUserCutDef" ]
  toJSON (UserCutDef uc) = object [ "IsUserCutDefined" .= String "UserCutDef"
                                  , "CutDetail" .= toJSON uc ]

-- | 

instance FromJSON UserCutSet where
  parseJSON (Object m) = do t <- elookup "IsUserCutDefined" m 
                            case t of 
                              String "NoUserCutDef" -> return NoUserCutDef
                              String "UserCutDef" -> do 
                                d <- elookup "CutDetail" m
                                uc <- parseJSON d 
                                return (UserCutDef uc)
                              _ -> fail "UserCutSet not parsed"
  parseJSON _ = fail "UserCutSet not parsed"

-- | 

instance ToJSON UserCut where
  toJSON (UserCut met etacutlep etcutlep etacutjet etcutjet) 
    = Array . V.fromList . map toJSON $ [met,etacutlep,etcutlep,etacutjet,etcutjet]

-- | 

instance FromJSON UserCut where
  parseJSON (Array v) | V.length v == 5 = do
    met       <- parseJSON (v V.! 0)
    etacutlep <- parseJSON (v V.! 1)
    etcutlep  <- parseJSON (v V.! 2)
    etacutjet <- parseJSON (v V.! 3)
    etcutjet  <- parseJSON (v V.! 4) 
    return (UserCut met etacutlep etcutlep etacutjet etcutjet)
                      | otherwise = fail "UserCut not parsed"
  parseJSON _ = fail "UserCut not parsed"
 
-- | 

instance ToJSON PGSType where
  toJSON NoPGS  = "NoPGS"
  toJSON RunPGS = "RunPGS"
  toJSON RunPGSNoTau = "RunPGSNoTau"

-- | 

instance FromJSON PGSType where
  parseJSON (String "NoPGS") = return NoPGS
  parseJSON (String "RunPGS") = return RunPGS
  parseJSON (String "RunPGSNoTau") = return RunPGSNoTau
  parseJSON _ = fail "PGSType not parsed"

-- | 

instance ToJSON MadGraphVersion where
  toJSON MadGraph4 = "MadGraph4"
  toJSON MadGraph5 = "MadGraph5"

-- | 

instance FromJSON MadGraphVersion where
  parseJSON (String "MadGraph4") = return MadGraph4
  parseJSON (String "MadGraph5") = return MadGraph5
  parseJSON _ = fail "MadGraphVersion not parsed"

-- | 

instance (Model a) => ToJSON (ModelParam a) where
  toJSON p = let str = briefParamShow p  
              in  String (pack str) 

-- | 

instance (Model a) => FromJSON (ModelParam a) where
  parseJSON (String str) = return . interpreteParam . unpack $ str
  parseJSON _ = fail "ModelParam not parsed"

-- | 

modelFromJSON :: (Model a) => Value -> Parser a 
modelFromJSON (String str) = maybe (fail "modelFromJSON failed") return $ modelFromString . unpack $ str
modelFromJSON _ = fail "modelFromJSON failed"

-- |

instance (Model a) => ToJSON (ProcessSetup a) where
  toJSON p = object [ "model" .= ( atomizeStr . modelName . model $ p )
                    , "process" .= ( atomizeStr . process $ p )
                    , "processBrief" .= ( atomizeStr . processBrief $ p )
                    , "workname" .= ( atomizeStr . workname $ p) ]

-- | 

instance (Model a) => FromJSON (ProcessSetup a) where
  parseJSON (Object m) = PS <$> (elookup "model" m >>= modelFromJSON)
                            <*> lookupfunc "process" m
                            <*> lookupfunc "processBrief" m
                            <*> lookupfunc "workname" m
  parseJSON _ = fail "ProcessSetup not parsed"
 
-- |

instance (Model a) => ToJSON (RunSetup a) where
  toJSON p = object [ "param" .= (toJSON . param $ p)
                    , "numevent"  .= (toJSON . numevent $ p)
                    , "machine"   .= (toJSON . machine $ p)
                    , "rgrun"     .= (toJSON . rgrun $ p)
                    , "rgscale"   .= (toJSON . rgscale $ p)
                    , "match"     .= (toJSON . match $ p)
                    , "cut"       .= (toJSON . cut $ p) 
                    , "pythia"    .= (toJSON . pythia $ p)
                    , "usercut"   .= (toJSON . usercut $ p)
                    , "lhesanitizer" .= (G.toJSON . lhesanitizer $ p)
                    , "pgs"       .= (toJSON . pgs $ p) 
                    , "jetalgo"   .= (G.toJSON . jetalgo $ p)
                    , "hep"       .= (G.toJSON . uploadhep $ p)
                    , "setnum"    .= (toJSON . setnum $ p) ] 

-- |

instance (Model a) => FromJSON (RunSetup a) where
  parseJSON (Object m) =   RS <$> lookupfunc "param" m   <*> lookupfunc "numevent" m
                              <*> lookupfunc "machine" m <*> lookupfunc "rgrun" m
                              <*> lookupfunc "rgscale" m <*> lookupfunc "match" m
                              <*> lookupfunc "cut" m     <*> lookupfunc "pythia" m
                              <*> lookupfunc "usercut" m <*> lookupfunc "lhesanitizer" m
                              <*> lookupfunc "pgs" m     <*> lookupfunc "jetalgo" m
                              <*> lookupfunc "hep" m     <*> lookupfunc "setnum" m
  parseJSON _ = fail "RunSetup not parsed"

-- | 

instance ToJSON EventSet where
  toJSON (EventSet p r) = object [ "psetup" .= toJSON p
                                 , "rsetup" .= toJSON r ] 

-- |

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
               EventSet <$> getPSetup mdl <*> getRSetup mdl 
          getPSetup :: (Model a) => a -> Parser (ProcessSetup a) 
          getPSetup _mdl = lookupfunc "psetup" m
          getRSetup :: (Model a) => a -> Parser (RunSetup a)
          getRSetup _mdl = lookupfunc "rsetup" m
  parseJSON _ = fail "EventSet not parsed"

-- |

instance ToJSON JobDetail where
  toJSON (EventGen evset rdir) = object [ "JobType" .= String "EventGen" 
                                        , "evset"   .= toJSON evset 
                                        , "rdir"    .= toJSON rdir ]
  toJSON (MathAnal mathanal evset rdir) = 
    object [ "JobType"  .= String "MathAnal" 
           , "mathanal" .= toJSON mathanal
           , "evset"    .= toJSON evset
           , "rdir"     .= toJSON rdir ]

-- |

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

-- | 

instance ToJSON WebDAVRemoteDir where
  toJSON (WebDAVRemoteDir rdir) = toJSON rdir 

-- |

instance FromJSON WebDAVRemoteDir where
  parseJSON v = WebDAVRemoteDir <$> parseJSON v

-- | 
   
instance ToJSON JobInfo where
  toJSON i = object [ "id"         .= (toJSON . jobinfo_id $ i)
                    , "detail"     .= (toJSON . jobinfo_detail $ i)
                    , "status"     .= (G.toJSON . jobinfo_status $ i) 
                    , "priority"   .= (G.toJSON . jobinfo_priority $ i) 
                    , "dependency" .= (toJSON . jobinfo_dependency $ i) ]

-- |

instance FromJSON JobInfo where
  parseJSON (Object m) = 
    JobInfo <$> lookupfunc "id" m
            <*> lookupfunc "detail" m
            <*> lookupfunc "status" m
            <*> lookupfunc "priority" m
            <*> lookupfunc "dependency" m 
  parseJSON _ = fail "JobInfo not parsed"
    
-- |
     
instance ToJSON ClientConfiguration where
  toJSON (ClientConfiguration computer math pbs montecarlo datasetdir) = 
      object [ "computer"    .= toJSON computer 
             , "mathematica" .= toJSON math
             , "pbs"         .= toJSON pbs   
             , "montecarlo"  .= toJSON montecarlo 
             , "datasetDir"  .= toJSON datasetdir ] 

-- |

instance FromJSON ClientConfiguration where
  parseJSON (Object m) = 
    ClientConfiguration 
      <$> lookupfunc "computer" m
      <*> lookupfunc "mathematica" m 
      <*> lookupfunc "pbs" m
      <*> lookupfunc "montecarlo" m
      <*> lookupfunc "datasetDir" m
  parseJSON _ = fail "ClientConfiguration not parsed"




--import Control.Applicative

{- 
class ToAeson a where 
  toAeson :: a -> Value

class FromAeson a where
  fromAeson :: Value -> Either String a -- Maybe a
-}


{-
instance (Data a) => ToAeson a where
  toAeson = G.toJSON

instance (Data a) => FromAeson a where
  fromAeson v = let r = G.parseJSON v 
                in case r of 
                     Success a -> return a 
                     Error _str -> Left $ (show . typeOf) (undefined :: a) ++ " is not parsed" -- Nothing  
-}

{-
instance ToAeson Bool where
  toAeson = Bool 

instance FromAeson Bool where
  fromAeson (Bool b) = return b
  fromAeson _ = Left "Bool not parsed"

instance ToAeson Int where
  toAeson = Number . I . fromIntegral

instance FromAeson Int where
  fromAeson (Number (I val)) = return (fromIntegral val) 
  fromAeson _ = Left "Int not parsed"

instance ToAeson Double where
  toAeson = Number . D

instance FromAeson Double where
  fromAeson (Number (D val)) = return val 
  fromAeson (Number (I val)) = return . fromIntegral $ val
  fromAeson _ = Left "Double not parsed"


instance (FromAeson a) => FromAeson [a] where
  fromAeson (Array vec) = mapM fromAeson (V.toList vec) 
  fromAeson _ = Left "[] not parsed"

instance (ToAeson a) => ToAeson [a] where
  toAeson = Array . V.fromList . map toAeson    

instance FromAeson String where
  fromAeson (String str) = return . unpack $ str  
  fromAeson _ = Left "String not parsed"

instance ToAeson String where
  toAeson = atomizeStr  
 
instance (ToAeson a, ToAeson b) => ToAeson (Either a b) where
  toAeson (Left a) = Object $ 
                        M.fromList [ ("Type", "Left")
                                   , ("Content", toAeson a) ] 
  toAeson (Right b) = Object $ 
                        M.fromList [ ("Type", "Right")
                                   , ("Content", toAeson b) ] 

instance (FromAeson a, FromAeson b) => FromAeson (Either a b) where
  fromAeson (Object m) = do t <- elookup "Type" m
                            case t of 
                              "Left"  -> Left  <$> (elookup "Content" m >>= fromAeson)
                              "Right" -> Right <$> (elookup "Content" m >>= fromAeson)
                              _ -> Left "Either not parsed" 
  fromAeson _ = Left "Either not parsed"
-}


{-
instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
  parseJSON (Array vec) = let lst = V.toList vec
                          in  if Prelude.length lst == 2 
                              then let a = lst !! 0 
                                       b = lst !! 1 
                                   in  (,) <$> parseJSON a <*> parseJSON b
                              else fail "(,) not parsed"
  parseJSON _ = fail "(,) not parsed"

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
  toJSON (a,b) = Array (V.fromList [toJSON a, toAeson b])
-}


{- 
parseJson :: (FromAeson a) => S.ByteString -> Either String a
parseJson bs =
  let resultjson = parse json bs
  in case resultjson of 
       Done _ rjson -> fromAeson rjson
       _            -> fail "parsing failed"
-}