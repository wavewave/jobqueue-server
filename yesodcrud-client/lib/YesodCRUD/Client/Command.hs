module YesodCRUD.Client.Command where

import YesodCRUD.Client.ProgType
import YesodCRUD.Client.Job
import YesodCRUD.Client.Config
import Data.Configurator

commandLineProcess :: Yesodcrud_client -> IO ()
commandLineProcess (Create cfg mn) = do 
  putStrLn "create called"
  mc <- getYesodcrudClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startCreate mn) mc
commandLineProcess (Get cfg n) = do 
  putStrLn "get called"
  mc <- getYesodcrudClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startGet n) mc
commandLineProcess (Put cfg n mn) = do 
  putStrLn "put called"
  mc <- getYesodcrudClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\c-> startPut c n mn) mc
commandLineProcess (Delete cfg n) = do 
  putStrLn "delete called"
  mc <- getYesodcrudClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startDelete n) mc
commandLineProcess (List cfg) = do 
  putStrLn "getlist called"
  mc <- getYesodcrudClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") startGetList mc
