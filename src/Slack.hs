{-# LANGUAGE OverloadedStrings #-}

module Slack where

import Data.Function ((&))
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as Tx
import qualified Network.HTTP.Simple as HTTP
import qualified Types as T


-- makeSlackRequest

makeSlackRequest' :: T.Config -> T.SlackMessagePayload -> IO Tx.Text
makeSlackRequest' config payload = do
    baseRequest <- HTTP.parseRequest "https://slack.com/api/chat.postMessage"
    let bearerToken = Tx.encodeUtf8 $ "Bearer " <> T.slackOauthToken config
        request =
            baseRequest
                & HTTP.setRequestBodyJSON payload
                & HTTP.setRequestMethod "POST"
                & HTTP.addRequestHeader "Authorization" bearerToken
    response <- HTTP.httpBS request

    HTTP.getResponseBody response
        & Tx.decodeUtf8
        & return


generateMessage' :: T.Config -> T.OrderBody -> T.SlackMessagePayload
generateMessage' config order_body =
    T.SlackMessagePayload
        (T.slackChannelId config)
        (makeMessage order_body)
    where
        makeMessage order_body = Tx.intercalate " "
            [ T.email order_body
            , "ordered"
            , Tx.pack . show . T.amount $ order_body
            , "cookies to"
            , T.address order_body
            , "for delivery"
            , T.deliveryTime order_body
            ]
