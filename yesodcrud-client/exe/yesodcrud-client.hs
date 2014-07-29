module Main where

import System.Console.CmdArgs

import YesodCRUD.Client.ProgType
import YesodCRUD.Client.Command

main :: IO () 
main = do 
  putStrLn "yesodcrud-client"
  param <- cmdArgs mode
  commandLineProcess param
