module HEP.Automation.JobQueue.Server.Work where

import Text.Parsec 
import Control.Monad.Identity
import Control.Exception (bracket)

import HEP.Parser.Config
import HEP.Storage.WebDAV.Type 
import HEP.Automation.JobQueue.Server.Type

import System.IO

serverConfigParser :: FilePath -> IO ServerConfig
serverConfigParser fp = do 
  putStrLn ("parsing server config file " ++ fp )
  bracket (openFile fp ReadMode) hClose $ \fh -> do 
    str <- hGetContents fh -- readFile fp
    let r = parse configServer "" str
    r `seq` case r of 
              Left msg -> error (show msg)
              Right sconf -> return $! sconf 
  

configServer :: ParsecT String () Identity ServerConfig
configServer = do 
  oneGroupFieldInput "server" $ do 
    url <- oneFieldInput "mainURL"
    webdavurl <- oneFieldInput "webdavURL"
    return (ServerConfig url (WebDAVServer webdavurl))
