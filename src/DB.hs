{-# LANGUAGE OverloadedStrings #-}

module DB where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Database.SQLite.Simple as Sql


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


-- addOrder :: Sql.Connection -> T.OrderBody -> IO ()
-- addOrder conn order =
--     Sql.execute conn
--         "insert into orders (amount, email, address, delivery_time) values (?, ?, ?, ?)"
--         ( T.amount order :: Int
--         , T.email order :: Text.Text
--         , T.address order :: Text.Text
--         , T.deliveryTime order :: Text.Text
--         )


saveOrder :: Sql.Connection -> [(T.Text, T.Text)] -> IO ()
saveOrder conn orderParameters =
    -- Showcasing the dumb, simple way to save the order info to the database.
    --
    -- We don't use a more structured type here, because SQLite does not have real
    -- column types anyway.
    -- Or course the right way to handle orders would be to have a validation
    -- step beforehand, that makes sure some fields are not empty, and the correct type.
    Sql.execute conn
        "insert into orders (amount, email, address, delivery_time) values (?, ?, ?, ?)"
        ( safeGet "amount" orderParamsMap
        , safeGet "email" orderParamsMap
        , safeGet "address" orderParamsMap
        , safeGet "delivery_time" orderParamsMap
        )
    where
        orderParamsMap = Map.fromList orderParameters
        safeGet k dict = Map.findWithDefault "" k dict
