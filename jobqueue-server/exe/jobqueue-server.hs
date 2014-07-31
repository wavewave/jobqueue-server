{-# LANGUAGE OverloadedStrings #-}

module Main where

import YesodCRUD.Server.Type
import YesodCRUD.Server.Yesod ()
import Yesod
import qualified Data.Map as M

main :: IO ()
main = do 
  putStrLn "yesodcrud-server"
  warp 7800 (YesodcrudServer "server.db")
