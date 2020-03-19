{-# LANGUAGE OverloadedStrings #-}

module DB where

import qualified Database.SQLite.Simple as Sql
-- import Database.SQLite.Simple (NamedParam(..))
-- import Database.SQLite.Simple.FromRow (FromRow)


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
