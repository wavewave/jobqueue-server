module HEP.Automation.JobQueue.Server.Work where

import Text.Parsec 
import HEP.Parser.Config
import Control.Monad.Identity
import Control.Applicative

import HEP.Parser.Config
import HEP.Storage.WebDAV.Type 
import HEP.Automation.JobQueue.Server.Type

serverConfigParser :: FilePath -> IO ServerConfig
serverConfigParser fp = do 
  putStrLn ("parsing server config file " ++ fp )
  str <- readFile fp
  let r = parse configServer "" str
  case r of 
    Left msg -> error (show msg)
    Right sconf -> return sconf 

configServer :: ParsecT String () Identity ServerConfig
configServer = do 
  oneGroupFieldInput "server" $ do 
    url <- oneFieldInput "webdavURL"
    return (ServerConfig (WebDAVServer url))
