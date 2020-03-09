{-# LANGUAGE OverloadedStrings #-}

module DB where

import Control.Applicative ()
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import           Database.SQLite.Simple (NamedParam(..))
import           Database.SQLite.Simple.FromRow (FromRow)

data TestField = TestField Int T.Text deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> SQL.field <*> SQL.field

instance SQL.ToRow TestField where
    toRow (TestField id_ str) = SQL.toRow (id_, str)

main :: IO ()
main = do
    conn <- SQL.open "test.db"
    SQL.execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
    SQL.execute conn "INSERT INTO test (str) VALUES (?)" (SQL.Only ("test string 2" :: String))
    SQL.execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
    rowId <- SQL.lastInsertRowId conn
    SQL.executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
    r <- SQL.query_ conn "SELECT * from test" :: IO [TestField]
    mapM_ print r
    SQL.execute conn "DELETE FROM test WHERE id = ?" (SQL.Only rowId)
    SQL.close conn
