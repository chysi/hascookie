{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Control.Monad.Trans (liftIO)
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Middleware.Static as Static
import qualified Web.Scotty as Scotty

import qualified DB
import qualified Types as T


main :: IO ()
main = do
    dbConnection <- DB.init "orders.db"

    Scotty.scotty 8080 $ do
        Scotty.middleware RequestLogger.logStdoutDev
        Scotty.middleware $ Static.staticPolicy (Static.addBase "site")

        Scotty.get "/" $
            Scotty.file "site/index.html"

        Scotty.get "/hello-app" $
            Scotty.text "I'm alive, thanks for asking!"

        Scotty.post "/icanhascookie" $ do
            liftIO $ putStrLn ">> order posted"
            (params :: T.OrderBody) <- Scotty.jsonData
            liftIO $ DB.addOrder dbConnection params
            Scotty.status Http.status200
            Scotty.text "Order submitted"

        Scotty.get "/orderstatus/:order_id" $ do
            orderId <- Scotty.param "order_id" :: Scotty.ActionM Int
            liftIO $ putStrLn $ ">> status for order nr. " <> show
                    (orderId :: Int)
            -- TODO: implement db call
            Scotty.status Http.status501
