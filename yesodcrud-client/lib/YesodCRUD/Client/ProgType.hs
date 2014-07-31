{-# LANGUAGE DeriveDataTypeable #-}

module YesodCRUD.Client.ProgType where 

-- import System.FilePath
import System.Console.CmdArgs hiding (name)

data Yesodcrud_client 
       = Create { config :: FilePath, name :: String }
       | Get    { config :: FilePath, uuid :: String } 
       | Put    { config :: FilePath, uuid :: FilePath, name :: String } 
       | Delete { config :: FilePath, uuid :: String } 
       | List   { config :: FilePath } 
       deriving (Show,Data,Typeable)

create :: Yesodcrud_client
create = Create { config = "test.conf"
                , name = "" &= typ "NAME"
                }

get :: Yesodcrud_client 
get = Get { config = "test.conf" 
          , uuid = "" &= typ "UUID"
          } 

put :: Yesodcrud_client 
put = Put { config = "test.conf"
          , uuid = "" &= typ "UUID" 
          , name = "" &= typ "NAME"
          }

delete :: Yesodcrud_client 
delete = Delete { config = "test.conf"
                , uuid = "" &= typ "UUID" 
                }

list :: Yesodcrud_client 
list = List { config = "test.conf" } 

mode :: Yesodcrud_client
mode = modes [ create, get, put, delete, list ]

