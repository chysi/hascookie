{-# LANGUAGE OverloadedStrings #-}

module DB where

import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Types as T
-- import Database.SQLite.Simple (NamedParam(..))
-- import Database.SQLite.Simple.FromRow (FromRow)


init :: String -> IO Sql.Connection
init dbFileName = do
    conn <- Sql.open dbFileName
    (ver : _) : _ <- Sql.query_ conn "select sqlite_version()" :: IO [[String]]
    putStrLn $ "sqlite version " <> ver
    Sql.execute_ conn
        "create table if not exists \
        \ orders (id integer primary key, amount integer, \
        \ email text, address text, delivery_time text)"
    pure conn


addOrder :: Sql.Connection -> T.OrderBody -> IO ()
addOrder conn order =
    Sql.execute conn
        "insert into orders (amount, email, address, delivery_time) values (?, ?, ?, ?)"
        ( T.amount order :: Int
        , T.email order :: Text.Text
        , T.address order :: Text.Text
        , T.deliveryTime order :: Text.Text
        )
