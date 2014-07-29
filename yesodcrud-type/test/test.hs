{-# LANGUAGE OverloadedStrings #-}

import Data.UUID.V4
import qualified Data.Text as T
import Database.Persist.Sqlite
import Database.Persist.Sql (rawQuery)
--
import YesodCRUD.Type

main = do 
  nuuid <- nextRandom
  let uuidtxt = T.pack ( show nuuid )

  -- runSqlite ":memory:" $ do
  runSqlite "test.db" $ do
    runMigration migrateCrud
    insert (CrudInfo uuidtxt "abcdefg") 

  return ()
