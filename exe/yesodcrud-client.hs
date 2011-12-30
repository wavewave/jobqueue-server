module Main where

import System.Console.CmdArgs

import Application.YesodCRUD.Client.ProgType
import Application.YesodCRUD.Client.Command

main :: IO () 
main = do 
  putStrLn "yesodcrud-client"
  param <- cmdArgs mode
  commandLineProcess param