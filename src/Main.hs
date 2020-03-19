{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Database.SQLite.Simple (Connection)
import qualified DB
import qualified Network.HTTP.Types as Http
-- import           Types
import           Web.Spock ((<//>))
import qualified Web.Spock as Spock
import qualified Web.Spock.Config as SC


type ServerDBConn  = ()
data ServerSession = ServerSession
data ServerState   = ServerState
    { dbConnection :: Connection
    }

type Server a = Spock.SpockM ServerDBConn ServerSession ServerState a


main :: IO ()
main = do
    dbConnection <- DB.init "orders.db"
    let serverState = ServerState { dbConnection = dbConnection }
    spockCfg <- SC.defaultSpockCfg ServerSession SC.PCNoDatabase serverState
    frontPage <- TIO.readFile "site/index.html"
    Spock.runSpock 8080 $ Spock.spock spockCfg (app frontPage)


app :: T.Text -> Server ()
app frontPage = do
    Spock.get Spock.root $ do
        liftIO $ putStrLn ">> main page requested"
        Spock.html frontPage

    Spock.get "heartbeat" $ do
        Spock.setStatus Http.status200

    Spock.post "icanhascookie" $ do
        liftIO $ putStrLn ">> order posted"
        -- params <- Spock.paramsPost
        -- let maybeOrderData = parseOrderBody params
        if False
            then do
                (ServerState dbConnection) <- Spock.getState
                liftIO $ DB.addOrder dbConnection 12345
                Spock.setStatus Http.status200
            else
                Spock.setStatus Http.status500
        -- Spock.text $ maybe "Invalid form" (T.pack . show) maybeOrderData

    Spock.get ("orderstatus" <//> Spock.var) $ \orderId -> do
        liftIO $ putStrLn $ ">> status for order nr. " <> show (orderId :: Int)
        -- TODO
        Spock.setStatus Http.status501
