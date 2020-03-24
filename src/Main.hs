{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad.Trans (liftIO)
import qualified Data.Text as Tx
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Middleware.Static as Static
import qualified System.Environment as Env
import qualified Web.Scotty as Scotty

import qualified DB
import qualified Slack
import qualified Types as T


envParseConfig :: IO (Either Tx.Text T.Config)
envParseConfig = do
    maybeChannelId  <- Env.lookupEnv "HASCOOKIE_SLACK_CHANNEL_ID"
    maybeOauthToken <- Env.lookupEnv "HASCOOKIE_SLACK_TOKEN"
    pure $ T.parseConfig maybeChannelId maybeOauthToken


main :: IO ()
main = do
    dbConnection <- DB.init "orders.db"

    -- maybeChannelId <- Env.lookupEnv "HASCOOKIE_SLACK_CHANNEL_ID"
    -- maybeAuthToken <- Env.lookupEnv "HASCOOKIE_SLACK_TOKEN"
    -- let maybeConf = tupleMaybe (maybeChannelId, maybeAuthToken)

    maybeConfig <- envParseConfig

    case maybeConfig of
        Left  err    -> TIO.putStrLn err
        Right config ->
            Scotty.scotty 8080 $ do
                Scotty.middleware RequestLogger.logStdoutDev
                Scotty.middleware $ Static.staticPolicy (Static.addBase "site")

                Scotty.get "/" $
                    Scotty.file "site/index.html"

                Scotty.get "/hello-app" $
                    Scotty.text "I'm alive, thanks for asking!"

                Scotty.post "/icanhas" $ do
                    params <- Scotty.params
                    liftIO $ DB.saveOrder dbConnection (paramsToStrict params)

                    -- slack_request_status <-
                    --     liftIO $ Slack.makeSlackRequest config $
                    --         Slack.generateMessage config params
                    -- liftIO $ TIO.putStrLn
                    --     $ ">> Slack response: " <> slack_request_status
                    -- TODO: check slack return message for success
                    -- Scotty.status Http.status200
                    Scotty.text "Order submitted"

                -- Scotty.post "/icanhascookie" $ do
                --     liftIO $ putStrLn ">> order posted"
                --     params <- Scotty.jsonData :: Scotty.ActionM T.OrderBody
                --     liftIO $ DB.addOrder dbConnection params

                --     slack_request_status <-
                --         liftIO $ Slack.makeSlackRequest' config $
                --             Slack.generateMessage' config params
                --     liftIO $ TIO.putStrLn
                --         $ ">> Slack request response: " <> slack_request_status
                --     -- TODO: check slack return message for success
                --     -- Scotty.status Http.status200
                --     Scotty.text "Order submitted"

                Scotty.get "/orderstatus/:order_id" $ do
                    orderId <- Scotty.param "order_id" :: Scotty.ActionM Int
                    liftIO $ putStrLn $ ">> status for order nr. " <> show (orderId :: Int)
                    -- TODO: implement db call
                    Scotty.status Http.status501


tupleMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
tupleMaybe (Just x, Just y) = Just (x, y)
tupleMaybe _ = Nothing


paramsToStrict :: [(TL.Text, TL.Text)] -> [(Tx.Text, Tx.Text)]
paramsToStrict params =
    map toStrict params
    where
        toStrict (a, b) = (TL.toStrict a, TL.toStrict b)
