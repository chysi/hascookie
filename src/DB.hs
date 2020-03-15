{-# LANGUAGE OverloadedStrings #-}

module DB where

import           Control.Applicative ()
import qualified Database.SQLite.Simple as Sql
-- import           Database.SQLite.Simple (NamedParam(..))
-- import           Database.SQLite.Simple.FromRow (FromRow)

-- data TestField = TestField Int T.Text deriving (Show)

-- instance FromRow TestField where
--     fromRow = TestField <$> Sql.field <*> Sql.field

-- instance Sql.ToRow TestField where
--     toRow (TestField id_ str) = Sql.toRow (id_, str)



-- main :: IO ()
-- main = do
--     conn <- Sql.open "test.db"
--     Sql.execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
--     Sql.execute conn "INSERT INTO test (str) VALUES (?)" (Sql.Only ("test string 2" :: String))
--     Sql.execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
--     rowId <- Sql.lastInsertRowId conn
--     Sql.executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
--     r <- Sql.query_ conn "SELECT * from test" :: IO [TestField]
--     mapM_ print r
--     Sql.execute conn "DELETE FROM test WHERE id = ?" (Sql.Only rowId)
--     Sql.close conn

init :: String -> IO Sql.Connection
init dbFileName = do
    conn <- Sql.open dbFileName
    (ver : _ ) : _ <- Sql.query_ conn "select sqlite_version()" :: IO [[String]]
    putStrLn $ "sqlite version " <> ver
    Sql.execute_ conn "create table if not exists orders (id integer primary key, amount integer)"
    pure conn

addOrder :: Sql.Connection -> Int -> IO ()
addOrder conn amount = do
    Sql.execute conn "insert into orders (amount) values (?)" (Sql.Only amount)
