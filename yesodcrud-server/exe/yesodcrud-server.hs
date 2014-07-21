{-# LANGUAGE OverloadedStrings #-}

module Main where

import Application.YesodCRUD.Server.Type
import Application.YesodCRUD.Server.Yesod ()
import Yesod
import qualified Data.Map as M
import Data.Acid 

main :: IO ()
main = do 
  putStrLn "yesodcrud-server"
  acid <- openLocalState M.empty 
  warp 7800 (YesodcrudServer acid)
