{-# LANGUAGE OverloadedStrings #-}

module Slack where

import           Data.Function                  ( (&) )
import qualified Network.HTTP.Simple           as HTTP
import qualified Types                         as T
import qualified Data.Text                     as Tx
import qualified Data.Text.Encoding            as Tx

makeSlackRequest :: T.Config -> T.SlackMessagePayload -> IO Tx.Text
makeSlackRequest config payload = do
  let url          = "https://slack.com/api/chat.postMessage"
      bearer_token = Tx.encodeUtf8 $ "Bearer " <> T.slackOauthToken config
  base_request <- HTTP.parseRequest url
  let request =
        base_request
          & HTTP.setRequestBodyJSON payload
          & HTTP.setRequestMethod "POST"
          & HTTP.addRequestHeader "Authorization" bearer_token
  response <- HTTP.httpBS request
  return $ Tx.decodeUtf8 $ HTTP.getResponseBody response

generateMessage :: T.Config -> T.OrderBody -> T.SlackMessagePayload
generateMessage config order_body = T.SlackMessagePayload
  (T.slackChannelId config)
  (makeMessage order_body)
 where
  makeMessage order_body = Tx.intercalate
    " "
    [ T.email order_body
    , "ordered"
    , Tx.pack . show . T.amount $ order_body
    , "cookies"
    , "to"
    , T.address order_body
    , "for delivery"
    , T.deliveryTime order_body
    ]
