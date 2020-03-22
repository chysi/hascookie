{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import           Control.Monad.Trans            ( liftIO )
import qualified Data.Text                     as Tx
import qualified Data.Text.IO                  as Tx
import qualified Network.HTTP.Types            as Http
import qualified Network.Wai.Middleware.RequestLogger
                                               as RequestLogger
import qualified Network.Wai.Middleware.Static as Static
import qualified System.Environment            as Env
import qualified Web.Scotty                    as Scotty

import qualified DB
import qualified Slack
import qualified Types                         as T


envParseConfig :: IO (Either Tx.Text T.Config)
envParseConfig = do
  maybe_channel_id  <- Env.lookupEnv "HASCOOKIE_SLACK_CHANNEL_ID"
  maybe_oauth_token <- Env.lookupEnv "HASCOOKIE_SLACK_TOKEN"
  return $ T.parseConfig maybe_channel_id maybe_oauth_token

main :: IO ()
main = do
  dbConnection <- DB.init "orders.db"
  maybeConfig  <- envParseConfig
  case maybeConfig of
    Left  err    -> Tx.putStrLn err
    Right config -> Scotty.scotty 8080 $ do
      Scotty.middleware RequestLogger.logStdoutDev
      Scotty.middleware $ Static.staticPolicy (Static.addBase "site")

      Scotty.get "/" $ Scotty.file "site/index.html"

      Scotty.get "/hello-app" $ Scotty.text "I'm alive, thanks for asking!"

      Scotty.post "/icanhascookie" $ do
        liftIO $ putStrLn ">> order posted"
        (params :: T.OrderBody) <- Scotty.jsonData
        liftIO $ DB.addOrder dbConnection params
        slack_request_status <-
          liftIO $ Slack.makeSlackRequest config $ Slack.generateMessage
            config
            params
        liftIO
          $  Tx.putStrLn
          $  ">> Slack request response: "
          <> slack_request_status
        Scotty.status Http.status200
        Scotty.text "Order submitted"

      Scotty.get "/orderstatus/:order_id" $ do
        orderId <- Scotty.param "order_id" :: Scotty.ActionM Int
        liftIO $ putStrLn $ ">> status for order nr. " <> show (orderId :: Int)
        -- TODO: implement db call
        Scotty.status Http.status501
