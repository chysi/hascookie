{-# LANGUAGE OverloadedStrings #-}

module Main where


import Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Network.Wai.Middleware.Static as Static
import qualified System.Environment as Env
import qualified Web.Scotty as Scotty
import qualified Data.Map.Strict as Map

import qualified DB
import qualified Slack
main :: IO ()
main = do
    dbConnection <- DB.init "orders.db"
    maybeAuthToken <- Env.lookupEnv "HASCOOKIE_SLACK_TOKEN"
    maybeChannelId <- Env.lookupEnv "HASCOOKIE_SLACK_CHANNEL_ID"

    case (maybeAuthToken, maybeChannelId) of
        (Just authTokenText, Just channelIdText) ->
            Scotty.scotty 8080 $ do

                Scotty.middleware RequestLogger.logStdoutDev
                Scotty.middleware $ Static.staticPolicy (Static.addBase "site")

                Scotty.get "/" $
                    Scotty.file "site/index.html"

                Scotty.get "/hello-app" $
                    Scotty.text "I'm alive, thanks for asking!"

                Scotty.post "/icanhas" $ do
                    params <- Scotty.params
                    let orderParamsMap = Map.fromList $ paramsToStrict params

                    liftIO $ DB.saveOrder dbConnection orderParamsMap

                    slackResponse <- liftIO $
                        Slack.makeSlackRequest orderParamsMap
                            (Slack.OAuthToken $ T.pack authTokenText)
                            (Slack.Channel $ T.pack channelIdText)
                    liftIO $ TIO.putStrLn
                        $ ">> Slack response: " <> slackResponse

                    -- slack_request_status <-
                    --     liftIO $ Slack.makeSlackRequest config $
                    --         Slack.generateMessage config params
                    -- liftIO $ TIO.putStrLn
                    --     $ ">> Slack response: " <> slack_request_status
                    -- TODO: check slack return message for success
                    -- Scotty.status Http.status200
                    Scotty.html "<h1>Order submitted</h1> congratulations!!"

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
        _ ->
            -- error
            TIO.putStrLn
                "Won't start without Slack integration, make sure\
                \ HASCOOKIE_SLACK_TOKEN and HASCOOKIE_SLACK_CHANNEL_ID\
                \ are set."


tupleMaybe :: (Maybe a, Maybe b) -> Maybe (a, b)
tupleMaybe (Just x, Just y) = Just (x, y)
tupleMaybe _ = Nothing


paramsToStrict :: [(TL.Text, TL.Text)] -> [(T.Text, T.Text)]
paramsToStrict params =
    map toStrict params
    where
        toStrict (a, b) = (TL.toStrict a, TL.toStrict b)
