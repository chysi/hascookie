{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NamedFieldPuns #-}

module Main where


import Control.Monad.Trans (liftIO)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Middleware.Static as Static
import Web.Spock ((<//>))
import qualified Web.Spock as Spock
import qualified Web.Spock.Config as SC

import qualified DB
-- import Types


type ServerDBConn  = ()

data ServerSession = ServerSession

data ServerState   = ServerState
    { dbConnection :: Database.SQLite.Simple.Connection
    }


main :: IO ()
main = do
    dbConnection <- DB.init "orders.db"
    let serverState = ServerState { dbConnection = dbConnection }
    spockCfg <- SC.defaultSpockCfg ServerSession SC.PCNoDatabase serverState
    Spock.runSpock 8080 $ Spock.spock spockCfg app


app :: Spock.SpockM ServerDBConn ServerSession ServerState ()
app = do
    Spock.middleware $ Static.staticPolicy (Static.addBase "static")
    Spock.get "/" $ do
        -- liftIO $ putStrLn ">> main page requested"
        Spock.file "text/html" "static/index.html"

    Spock.get "hello-app" $ do
        Spock.text "I'm alive, thanks for asking!"

    Spock.post "icanhascookie" $ do
        liftIO $ putStrLn ">> order posted"
        -- TODO: implement db saving
        -- params <- Spock.paramsPost
        -- let maybeOrderData = parseOrderBody params
        if False
            then do
                ServerState { dbConnection = dbConnection } <- Spock.getState
                liftIO $ DB.addOrder dbConnection 12345
                Spock.setStatus Http.status200
            else
                Spock.setStatus Http.status500
        -- Spock.text $ maybe "Invalid form" (T.pack . show) maybeOrderData

    Spock.get ("orderstatus" <//> Spock.var) $ \orderId -> do
        liftIO $ putStrLn $ ">> status for order nr. " <> show (orderId :: Int)
        -- TODO: implement db call
        Spock.setStatus Http.status501
