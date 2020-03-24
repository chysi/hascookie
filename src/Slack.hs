{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Slack where

import qualified Data.Aeson as A
import Data.Function ((&))
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as Tx
import qualified Network.HTTP.Simple as HTTP
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map


makeSlackRequest
    :: Map.Map Tx.Text Tx.Text
    -> OAuthToken
    -> Channel
    -> IO Tx.Text
makeSlackRequest orderParamsMap (OAuthToken token) (Channel channelId) = do
    baseRequest <- HTTP.parseRequest "https://slack.com/api/chat.postMessage"
    let bearerToken = Tx.encodeUtf8 $ "Bearer " <> token
        request =
            baseRequest
                & HTTP.setRequestBodyJSON payload
                & HTTP.setRequestMethod "POST"
                & HTTP.addRequestHeader "Authorization" bearerToken
    response <- HTTP.httpBS request

    HTTP.getResponseBody response
        & Tx.decodeUtf8
        & return
    where
        payload :: MessagePayload
        payload = MessagePayload channelId messageBody

        messageBody = Tx.intercalate " "
            [ safeGet "email" orderParamsMap
            , "ordered"
            , safeGet "amount" orderParamsMap
            , "cookies to"
            , safeGet "address" orderParamsMap
            , "for delivery"
            , safeGet "delivery_time" orderParamsMap
            ]

        safeGet k dict = Map.findWithDefault "" k dict


data OAuthToken = OAuthToken Tx.Text

data Channel = Channel Tx.Text

data MessagePayload = MessagePayload
    { channel :: Tx.Text
    , text :: Tx.Text
    }
    deriving (Show, Eq, Ord, Generic)

encodingOptions :: A.Options
encodingOptions =
    A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' }

instance A.ToJSON MessagePayload where
    toEncoding =
        A.genericToEncoding encodingOptions


-- makeSlackRequest' :: T.Config -> T.SlackMessagePayload -> IO Tx.Text
-- makeSlackRequest' config payload = do
--     baseRequest <- HTTP.parseRequest "https://slack.com/api/chat.postMessage"
--     let bearerToken = Tx.encodeUtf8 $ "Bearer " <> T.slackOauthToken config
--         request =
--             baseRequest
--                 & HTTP.setRequestBodyJSON payload
--                 & HTTP.setRequestMethod "POST"
--                 & HTTP.addRequestHeader "Authorization" bearerToken
--     response <- HTTP.httpBS request

--     HTTP.getResponseBody response
--         & Tx.decodeUtf8
--         & return


-- generateMessage' :: T.Config -> T.OrderBody -> T.SlackMessagePayload
-- generateMessage' config order_body =
--     T.SlackMessagePayload
--         (T.slackChannelId config)
--         (makeMessage order_body)
--     where
--         makeMessage order_body = Tx.intercalate " "
--             [ T.email order_body
--             , "ordered"
--             , Tx.pack . show . T.amount $ order_body
--             , "cookies to"
--             , T.address order_body
--             , "for delivery"
--             , T.deliveryTime order_body
--             ]
