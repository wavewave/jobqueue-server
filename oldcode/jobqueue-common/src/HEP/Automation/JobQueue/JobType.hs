{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      : HEP.Automation.JobQueue.JobType 
-- Copyright   : (c) 2011, 2012, 2014 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- 
-- Types for a event gen job
--
----------------------------------------------------

module HEP.Automation.JobQueue.JobType where

import Control.Applicative
import Data.SafeCopy
import Data.Serialize.Get
import Data.Typeable
import Data.Data
--
import HEP.Automation.MadGraph.Card
import HEP.Automation.MadGraph.Model
import HEP.Automation.MadGraph.ModelParser
import HEP.Automation.MadGraph.Run
import HEP.Automation.MadGraph.SetupType
import HEP.Automation.MadGraph.Type
import HEP.Automation.EventGeneration.Type

import HEP.Parser.LHE.Sanitizer.Type
import HEP.Storage.WebDAV.Type 
 


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
  putCopy (LHC7 detector)   = contain $ do {safePut (1 :: Int); safePut detector}
  putCopy (LHC8 detector)   = contain $ do {safePut (2 :: Int); safePut detector} 
  putCopy (LHC10 detector)  = contain $ do {safePut (3 :: Int); safePut detector} 
  putCopy (LHC13 detector)  = contain $ do {safePut (4 :: Int); safePut detector} 
  putCopy (LHC14 detector)  = contain $ do {safePut (5 :: Int); safePut detector} 
  putCopy (Parton energy detector) = 
    contain $ do {safePut (6 :: Int); safePut energy; safePut detector}
  putCopy (PolParton energy ipol detector) = 
    contain $ do safePut (7 :: Int) 
                 safePut energy 
                 (safePut . rhpol_percent . particle1pol ) ipol 
                 (safePut . rhpol_percent . particle2pol ) ipol 
                 safePut detector

  getCopy = 
    contain $ do 
      (x :: Int) <- safeGet 
      case x of 
        0 -> return TeVatron 
        1 -> LHC7 <$> safeGet
        2 -> LHC8 <$> safeGet
        3 -> LHC10 <$> safeGet
        4 -> LHC13 <$> safeGet
        5 -> LHC14 <$> safeGet
        6 -> Parton <$> safeGet <*> safeGet
        7 -> do energy <- safeGet
                p1pol  <- RH <$> safeGet 
                p2pol  <- RH <$> safeGet
                let ipol = InitPolarization p1pol p2pol 
                detector <- safeGet 
                return (PolParton energy ipol detector)

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


instance SafeCopy PGSType where
  putCopy NoPGS                = contain (safePut (0 :: Int))
  putCopy (RunPGS algotau)     = contain $ do 
                                   safePut (1 :: Int)
                                   safePut algotau
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           0 -> return NoPGS
                           1 -> do algotau <- safeGet
                                   return (RunPGS algotau)

instance SafeCopy PGSTau where
  -- putCopy NoTau   = contain (safePut (0 :: Int))
  putCopy WithTau = contain (safePut (1 :: Int))
  getCopy = contain $ do (x :: Int) <- safeGet 
                         case x of 
                           -- 0 -> return NoTau
                           1 -> return WithTau



instance SafeCopy EventSet where
  putCopy (EventSet p param r) = 
    let PS m pr pb wn hs = p 
        RS ne ma rgr rgs mat cu py ls pg hu sn = r 
    in  contain $ do safePut (modelName m)  
                     safePut pr  
                     safePut pb 
                     safePut wn
                     safePut hs
                     safePut param
                     safePut ne 
                     safePut ma 
                     safePut rgr 
                     safePut rgs
                     safePut mat 
                     safePut cu 
                     safePut py 
                     safePut ls
                     safePut pg 
                     safePut hu
                     safePut sn
  getCopy = contain $ do 
    modelstr <- safeGet 
    pr <- safeGet  
    pb <- safeGet 
    wn <- safeGet 
    hs <- safeGet

    let mkEventSet :: ModelBox -> Get EventSet
        mkEventSet (ModelBox mdl) = 
            EventSet <$> getPSetup mdl <*> safeGet <*> getRSetup

        getPSetup :: (Model a) => a -> Get (ProcessSetup a)
        getPSetup mdl = return (PS mdl pr pb wn hs)

        getRSetup :: Get RunSetup
        getRSetup = RS <$> safeGet <*> safeGet <*> safeGet <*> safeGet 
                       <*> safeGet <*> safeGet <*> safeGet <*> safeGet 
                       <*> safeGet <*> safeGet <*> safeGet

    let maybemodelbox = modelParse modelstr 
    case maybemodelbox of 
      Just modelbox -> mkEventSet modelbox 
      Nothing -> error $ "modelname : " ++ modelstr ++ " is strange!"
 
instance SafeCopy MGProcess where
  putCopy p = contain (safePut (mgp_definelines p) >> safePut (mgp_processes p))
  getCopy = contain $ MGProc <$> safeGet <*> safeGet 

instance SafeCopy HashSalt where
  putCopy s = contain (safePut (unHashSalt s))
  getCopy = contain $ HashSalt <$> safeGet

instance SafeCopy SanitizeCmd where
  putCopy (Eliminate xs) = contain (safePut (1 :: Int) >> safePut xs)
  putCopy (Replace xs)   = contain (safePut (2 :: Int) >> safePut xs)
  putCopy Shuffle        = contain (safePut (3 :: Int))
  putCopy Blobize        = contain (safePut (4 :: Int))
  getCopy = contain $ do (x :: Int) <- safeGet
                         case x of 
                           1 -> Eliminate <$> safeGet
                           2 -> Replace <$> safeGet
                           3 -> return Shuffle
                           4 -> return Blobize

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



