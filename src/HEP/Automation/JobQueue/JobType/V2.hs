{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

----------------------------------------------------
--
-- Module       : HEP.Automation.JobQueue.JobType
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

module HEP.Automation.JobQueue.JobType.V2 where

import Control.Applicative

import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.Machine.V2
import HEP.Automation.MadGraph.UserCut
import HEP.Automation.MadGraph.SetupType.V2

import HEP.Automation.MadGraph.ModelParser

{-import HEP.Automation.MadGraph.Model.AxiGluon 
import HEP.Automation.MadGraph.Model.Octet -}

import HEP.Storage.WebDAV.Type 

import Data.SafeCopy
import Data.Serialize.Get


data EventSet = forall a. (Model a) => 
  EventSet {
    evset_psetup :: ProcessSetup a, 
    evset_rsetup :: RunSetup a
  } 

instance Show EventSet where
  show (EventSet p r) = show p ++ "\n" ++ show r 


instance (Model a) => SafeCopy (ModelParam a) where
  putCopy mp = contain (safePut (briefParamShow mp))
  getCopy = contain $ do 
              str <- safeGet
              return (interpreteParam str)

instance SafeCopy Detector where
  putCopy Tevatron = contain (safePut (1 :: Int))
  putCopy LHC = contain (safePut (2 :: Int))
  putCopy CMS = contain (safePut (3 :: Int))
  putCopy ATLAS = contain (safePut (4 :: Int))
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           1 -> return Tevatron
                           2 -> return LHC
                           3 -> return CMS
                           4 -> return ATLAS

instance SafeCopy MachineType where 
  putCopy TeVatron = contain (safePut (0 :: Int))
  putCopy LHC7  = contain $ do {safePut (1 :: Int)}
  putCopy LHC14 = contain $ do {safePut (2 :: Int)} 
  putCopy (Parton energy detector) = 
    contain $ do {safePut (3 :: Int); safePut energy; safePut detector}
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return TeVatron 
                           1 -> return LHC7
                           2 -> return LHC14 
                           3 -> Parton <$> safeGet <*> safeGet

instance SafeCopy RGRunType where
  putCopy Fixed = contain (safePut (0 :: Int)) 
  putCopy Auto  = contain (safePut (1 :: Int)) 
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return Fixed
                           1 -> return Auto

instance SafeCopy MatchType where
  putCopy NoMatch = contain (safePut (0 :: Int))
  putCopy MLM     = contain (safePut (1 :: Int))
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return NoMatch
                           1 -> return MLM
 
instance SafeCopy CutType where
  putCopy NoCut  = contain (safePut (0 :: Int))
  putCopy DefCut = contain (safePut (1 :: Int))
  putCopy KCut   = contain (safePut (2 :: Int))
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return NoCut 
                           1 -> return DefCut 
                           2 -> return KCut

instance SafeCopy PYTHIAType where
  putCopy NoPYTHIA  = contain (safePut (0 :: Int))
  putCopy RunPYTHIA = contain (safePut (1 :: Int))
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return NoPYTHIA
                           1 -> return RunPYTHIA

instance SafeCopy UserCutSet where
  putCopy NoUserCutDef = contain (safePut (0 :: Int)) 
  putCopy (UserCutDef uc) = contain $ do {safePut (1 :: Int); safePut uc }
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return NoUserCutDef
                           1 -> do (uc :: UserCut) <- safeGet
                                   return (UserCutDef uc) 

instance SafeCopy UserCut where
  putCopy (UserCut met etacutlep etcutlep etacutjet etcutjet) = contain $ do {
    safePut met; safePut etacutlep; safePut etcutlep; safePut etacutjet; safePut etcutjet }
  getCopy = contain $ UserCut <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

instance SafeCopy PGSType where
  putCopy NoPGS       = contain (safePut (0 :: Int))
  putCopy RunPGS      = contain (safePut (1 :: Int))
  putCopy RunPGSNoTau = contain (safePut (2 :: Int))
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return NoPGS
                           1 -> return RunPGS
                           2 -> return RunPGSNoTau

instance SafeCopy EventSet where
  putCopy (EventSet p r) = 
    let PS mv m pr pb wn = p 
        RS mp ne ma rgr rgs mat cu py uc pg ja hu sn = r 
    in  contain $ do safePut mv 
                     safePut (modelName m)  
                     safePut pr  
                     safePut pb 
                     safePut wn
                     safePut mp 
                     safePut ne 
                     safePut ma 
                     safePut rgr 
                     safePut rgs
                     safePut mat 
                     safePut cu 
                     safePut py 
                     safePut uc 
                     safePut pg 
                     safePut ja
                     safePut hu
                     safePut sn
  getCopy = contain $ do 
    mv <- safeGet
    modelstr <- safeGet 
    pr <- safeGet  
    pb <- safeGet 
    wn <- safeGet 

    let mkEventSet :: ModelBox -> Get EventSet
        mkEventSet (ModelBox mdl) = 
            EventSet <$> getPSetup mdl <*> getRSetup mdl 

        getPSetup :: (Model a) => a -> Get (ProcessSetup a)
        getPSetup mdl = return (PS mv mdl pr pb wn)

        getRSetup :: (Model a) => a -> Get (RunSetup a)
        getRSetup _mdl = RS <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet 
                            <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet 
                            <*> safeGet <*> safeGet <*> safeGet

    let maybemodelbox = modelParse modelstr 
    case maybemodelbox of 
      Just modelbox -> mkEventSet modelbox 
      Nothing -> error $ "modelname : " ++ modelstr ++ " is strange!"
 
instance SafeCopy PGSJetAlgorithm where
  putCopy (Cone conesize) = contain $ do {safePut (1 :: Int); safePut conesize} 
  putCopy (KTJet conesize) = contain $ do {safePut (2 :: Int); safePut conesize}
  putCopy (AntiKTJet conesize) = contain $ do {safePut (3 :: Int); safePut conesize}
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of
                           1 -> Cone <$> safeGet
                           2 -> KTJet <$> safeGet
                           3 -> AntiKTJet <$> safeGet


instance SafeCopy HEPFileType where
  putCopy NoUploadHEP = contain $ safePut (1 :: Int) 
  putCopy UploadHEP = contain $ safePut (2 :: Int)
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of
                           1 -> return NoUploadHEP
                           2 -> return UploadHEP


instance SafeCopy MadGraphVersion where
  putCopy MadGraph4 = contain $ safePut (4 :: Int) 
  putCopy MadGraph5 = contain $ safePut (5 :: Int) 
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           4 -> return MadGraph4
                           5 -> return MadGraph5


instance SafeCopy WebDAVRemoteDir where
  putCopy (WebDAVRemoteDir str) = contain $ safePut str
  getCopy = contain $ WebDAVRemoteDir <$> safeGet



