{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad.Trans (liftIO)
import qualified Network.HTTP.Types as Http
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Network.Wai.Middleware.Static as Static
import qualified Web.Scotty as Scotty

import qualified DB
-- import Types


main :: IO ()
main = do
    dbConnection <- DB.init "orders.db"

    Scotty.scotty 8080 $ do
        Scotty.middleware logStdout
        Scotty.middleware $ Static.staticPolicy (Static.addBase "site")
        Scotty.get "/" $ do
            -- liftIO $ putStrLn ">> main page requested"
            Scotty.file "site/index.html"

        Scotty.get "hello-app" $ do
            Scotty.text "I'm alive, thanks for asking!"

        Scotty.post "icanhascookie" $ do
            liftIO $ putStrLn ">> order posted"
            -- TODO: implement db saving
            -- params <- Scotty.paramsPost
            -- let maybeOrderData = parseOrderBody params
            if False
                then do
                    -- ServerState { dbConnection = dbConnection } <- Scotty.getState
                    liftIO $ DB.addOrder dbConnection 12345
                    Scotty.status Http.status200
                else
                    Scotty.status Http.status500
            -- Scotty.text $ maybe "Invalid form" (T.pack . show) maybeOrderData

        Scotty.get "orderstatus/:order_id" $ do
            orderId <- Scotty.param "order_id" :: Scotty.ActionM Int
            liftIO $ putStrLn $ ">> status for order nr. " <> show (orderId :: Int)
            -- TODO: implement db call
            Scotty.status Http.status501
