{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, OverlappingInstances,
             UndecidableInstances #-}

-----------------------------------------------------
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

-- import HEP.Automation.MadGraph.Model.AxiGluon
import HEP.Automation.MadGraph.ModelParser

import HEP.Storage.WebDAV.Type

import Data.Text hiding (map)
import Data.Attoparsec 
import Data.Attoparsec.Number 
import Data.Aeson.Types hiding (parse)
import Data.Aeson.Parser 

import qualified Data.ByteString as S
import qualified Data.Vector as V
import qualified Data.Map as M

import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobQueue
import HEP.Automation.JobQueue.Config 

import Data.Data
import qualified Data.Aeson.Generic as G


--import Control.Applicative

class ToAeson a where 
  toAeson :: a -> Value

class FromAeson a where
  fromAeson :: Value -> Maybe a


atomize :: (Show a) => a -> Value 
atomize = atomizeStr . show 

atomizeStr :: String -> Value
atomizeStr = String . pack

instance (Data a) => ToAeson a where
  toAeson = G.toJSON

instance (Data a) => FromAeson a where
  fromAeson v = let r = G.fromJSON v 
                in case r of 
                     Success a -> return a 
                     Error _str -> Nothing  

instance ToAeson Bool where
  toAeson = Bool 

instance FromAeson Bool where
  fromAeson (Bool b) = Just b
  fromAeson _ = Nothing 

instance ToAeson Int where
  toAeson = Number . I . fromIntegral

instance FromAeson Int where
  fromAeson (Number (I val)) = Just (fromIntegral val) 
  fromAeson _ = Nothing

instance ToAeson Double where
  toAeson = Number . D

instance FromAeson Double where
  fromAeson (Number (D val)) = Just val 
  fromAeson (Number (I val)) = Just . fromIntegral $ val
  fromAeson _ = Nothing


instance (FromAeson a) => FromAeson [a] where
  fromAeson (Array vec) = mapM fromAeson (V.toList vec) 
  fromAeson _ = Nothing

instance (ToAeson a) => ToAeson [a] where
  toAeson = Array . V.fromList . map toAeson    

instance FromAeson String where
  fromAeson (String str) = Just . unpack $ str  
  fromAeson _ = Nothing 

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
  fromAeson (Object m) = do t <- M.lookup "Type" m
                            case t of 
                              "Left"  -> Left  <$> (M.lookup "Content" m >>= fromAeson)
                              "Right" -> Right <$> (M.lookup "Content" m >>= fromAeson)
                              _ -> Just (Left undefined) 
  fromAeson _ = Nothing

instance ToAeson MachineType where
  toAeson TeVatron = Object (M.singleton "Type" (String "TeVatron"))
  toAeson (LHC7 detector) = Object (M.fromList [ ("Type", (String "LHC7"))
                                               , ("Detector", toAeson detector)])
  toAeson (LHC14 detector)  = Object (M.fromList [ ("Type", (String "LHC14"))
                                                 , ("Detector", toAeson detector)])
  toAeson (Parton energy detector) = Object (M.fromList 
                                              [ ("Type",String "Parton")
                                              , ("Energy",Number (D energy))
                                              , ("Detector", toAeson detector)]) 

instance FromAeson MachineType where
  fromAeson (Object m) = do t <- M.lookup "Type" m
                            case t of 
                              String "TeVatron" -> return TeVatron 
                              String "LHC7"     -> LHC7 <$> lookupfunc "Detector"
                              String "LHC14"    -> LHC14 <$> lookupfunc "Detector"
                              String "Parton"   -> do 
                                Parton <$> lookupfunc "Energy" 
                                       <*> lookupfunc "Detector"
                              _ -> Nothing
    where lookupfunc str = M.lookup str m >>= fromAeson 
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
                                uc <- fromAeson d 
                                return (UserCutDef uc)
                              _ -> Nothing 
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
  toAeson p = let str = briefParamShow p  
              in  String (pack str) 

instance (Model a) => FromAeson (ModelParam a) where
  fromAeson (String str) = Just . interpreteParam . unpack $ str
  fromAeson _ = Nothing 

modelFromAeson :: (Model a) => Value -> Maybe a 
modelFromAeson (String str) = modelFromString . unpack $ str
modelFromAeson _ = Nothing


instance (Model a) => ToAeson (ProcessSetup a) where
  toAeson p = Object 
              $ M.fromList 
                    [ ("model"       , atomizeStr . modelName . model $ p)
                    , ("process"     , atomizeStr . process $ p)
                    , ("processBrief", atomizeStr . processBrief $ p)
                    , ("workname"    , atomizeStr . workname $ p) ]

instance (Model a) => FromAeson (ProcessSetup a) where
  fromAeson (Object m) =  
    PS <$> (M.lookup "model" m >>= modelFromAeson)
       <*> lookupfunc "process" 
       <*> lookupfunc "processBrief" 
       <*> lookupfunc "workname" 
    where lookupfunc str = M.lookup str m >>= fromAeson  
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
                    , ("lhesanitizer", toAeson . lhesanitizer $ p)
                    , ("pgs"      , toAeson . pgs $ p) 
                    , ("jetalgo"  , toAeson . jetalgo $ p)
                    , ("hep"      , toAeson . uploadhep $ p )
                    , ("setnum"   , Number . I . fromIntegral . setnum $ p)] 

instance (Model a) => FromAeson (RunSetup a) where
  fromAeson (Object m) = 
    RS <$> lookupfunc "param"   <*> lookupfunc "numevent" 
       <*> lookupfunc "machine" <*> lookupfunc "rgrun" 
       <*> lookupfunc "rgscale" <*> lookupfunc "match"
       <*> lookupfunc "cut"     <*> lookupfunc "pythia"  
       <*> lookupfunc "usercut" <*> lookupfunc "lhesanitizer"
       <*> lookupfunc "pgs"     <*> lookupfunc "jetalgo" 
       <*> lookupfunc "hep"     <*> lookupfunc "setnum"
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
          String str -> do 
            modelbox <- modelParse (unpack str) 
            mkEventSet modelbox   
          _ -> Nothing 
      _ -> Nothing
    where mkEventSet :: ModelBox -> Maybe EventSet
          mkEventSet (ModelBox mdl) = 
               EventSet <$> getPSetup mdl <*> getRSetup mdl 
          getPSetup :: (Model a) => a -> Maybe (ProcessSetup a) 
          getPSetup _mdl = lookupfunc "psetup" 
          getRSetup :: (Model a) => a -> Maybe (RunSetup a)
          getRSetup _mdl = lookupfunc "rsetup"
          lookupfunc str = M.lookup str m >>= fromAeson
  fromAeson _ = Nothing 


instance ToAeson JobDetail where
  toAeson (EventGen evset rdir) = Object $ 
                               M.fromList [ ( "JobType",  String "EventGen" ) 
                                          , ( "evset", toAeson evset) 
                                          , ( "rdir",  toAeson rdir) ]
  toAeson (MathAnal mathanal evset rdir) = 
    Object $ M.fromList [ ( "JobType",  String "MathAnal" )
                        , ( "mathanal", toAeson mathanal)
                        , ( "evset", toAeson evset)
                        , ( "rdir",  toAeson rdir) ]

instance FromAeson JobDetail where
  fromAeson (Object m) = do
    t <- M.lookup "JobType" m 
    case t of 
      "EventGen" -> EventGen <$> (M.lookup "evset" m >>= fromAeson) 
                             <*> (M.lookup "rdir" m >>= fromAeson)
      "MathAnal" -> MathAnal <$> (M.lookup "mathanal" m >>= fromAeson)
                             <*> (M.lookup "evset" m >>= fromAeson)
                             <*> (M.lookup "rdir" m >>= fromAeson)
      _ -> Nothing 
  fromAeson _ = Nothing


instance ToAeson WebDAVRemoteDir where
  toAeson (WebDAVRemoteDir rdir) = toAeson rdir 

instance FromAeson WebDAVRemoteDir where
  fromAeson v = WebDAVRemoteDir <$> fromAeson v

{-
instance ToAeson JobStatus where
  toAeson Unassigned = Object $ 
                         M.fromList [ ("Status", String "Unassigned") ] 
  toAeson Assigned client  = Object $ 
                              
                           
String "Assigned"
  toAeson BeingCalculated client = String "BeingCalculated"
  toAeson BeingTested client = String "BeingTested"
  toAeson Finished client = String "Finished"

instance FromAeson JobStatus where
  fromAeson (String "Unassigned") = Just Unassigned
  fromAeson (String "Assigned") = Just Assigned
  fromAeson (String "BeingCalculated") = Just BeingCalculated
  fromAeson (String "BeingTested") = Just BeingTested
  fromAeson (String "Finished") = Just Finished
  fromAeson _ = Nothing 
-}
   
instance ToAeson JobInfo where
  toAeson i = Object $ 
                M.fromList 
                  [ ("id" , toAeson . jobinfo_id $ i)
                  , ("detail", toAeson . jobinfo_detail $ i)
                  , ("status", toAeson . jobinfo_status $ i) 
                  , ("priority", toAeson . jobinfo_priority $ i) 
                  , ("dependency", toAeson . jobinfo_dependency $ i )
                  ]

instance FromAeson JobInfo where
  fromAeson (Object m) = 
    JobInfo <$> lookupfunc "id" 
            <*> lookupfunc "detail" 
            <*> lookupfunc "status"  
            <*> lookupfunc "priority"
            <*> lookupfunc "dependency" 
    where lookupfunc str = M.lookup str m >>= fromAeson  
  fromAeson _ = Nothing
         
instance ToAeson ClientConfiguration where
  toAeson (ClientConfiguration computer math pbs montecarlo datasetdir) = 
      Object $ M.fromList 
                 [ ("computer", toAeson computer )
                 , ("mathematica", Bool math)
                 , ("pbs", Bool pbs )  
                 , ("montecarlo", Bool montecarlo) 
                 , ("datasetDir", toAeson datasetdir) ] 

instance FromAeson ClientConfiguration where
  fromAeson (Object m) = 
    ClientConfiguration 
      <$> lookupfunc "computer" 
      <*> lookupfunc "mathematica" 
      <*> lookupfunc "pbs" 
      <*> lookupfunc "montecarlo"
      <*> lookupfunc "datasetDir"
    where lookupfunc str = M.lookup str m >>= fromAeson
  fromAeson _ = Nothing

instance (FromAeson a, FromAeson b) => FromAeson (a,b) where
  fromAeson (Array vec) = let lst = V.toList vec
                          in  if Prelude.length lst == 2 
                              then let a = lst !! 0 
                                       b = lst !! 1 
                                   in  (,) <$> fromAeson a <*> fromAeson b
                              else Nothing 

instance (ToAeson a, ToAeson b) => ToAeson (a,b) where
  toAeson (a,b) = Array (V.fromList [toAeson a, toAeson b])


parseJson :: (FromAeson a) => S.ByteString -> Maybe a
parseJson bs =
  let resultjson = parse json bs
  in case resultjson of 
       Done _ rjson -> fromAeson rjson
       _            -> Nothing 
