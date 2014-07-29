{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Conduit (($$))
import Data.Conduit.List as CL
import Data.Monoid ((<>))
import Data.UUID.V4
import qualified Data.Text as T
import Database.Persist.Sqlite
import Database.Persist.Sql (rawQuery)
--
import YesodCRUD.Type

main = do 
  nuuid <- nextRandom
  -- let uuidtxt = T.pack ( show nuuid )

  let ninfo = YesodcrudInfo nuuid "abcdefg"
  -- runSqlite ":memory:" $ do
  runSqlite "test.db" $ do
    runMigration migrateCrud
    insert (toCrudInfo ninfo) -- (CrudInfo uuidtxt "abcdefg")
    dumpdb "crud_info"

 

  return ()

dumpdb tablename = rawQuery ("select * from " <> tablename) [] $$ CL.mapM_ (liftIO . print)
