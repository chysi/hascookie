{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack where

import qualified Data.Aeson as A
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEncoding
import GHC.Generics (Generic)
import qualified Network.HTTP.Simple as HTTP


makeSlackRequest :: Map.Map T.Text T.Text -> OAuthToken -> Channel -> IO T.Text
makeSlackRequest orderParamsMap (OAuthToken token) (Channel channelId) = do
    baseRequest <- HTTP.parseRequest "https://slack.com/api/chat.postMessage"
    let bearerToken = TextEncoding.encodeUtf8 $ "Bearer " <> token
        request =
            baseRequest
                & HTTP.setRequestBodyJSON payload
                & HTTP.setRequestMethod "POST"
                & HTTP.addRequestHeader "Authorization" bearerToken
    response <- HTTP.httpBS request

    HTTP.getResponseBody response
        & TextEncoding.decodeUtf8
        & return
    where
        payload :: MessagePayload
        payload = MessagePayload channelId messageBody

        messageBody = T.intercalate " "
            [ safeGet "email" orderParamsMap
            , "ordered"
            , safeGet "amount" orderParamsMap
            , "cookies to"
            , safeGet "address" orderParamsMap
            , "for delivery"
            , safeGet "delivery_time" orderParamsMap
            ]

        safeGet k dict = Map.findWithDefault "" k dict


data OAuthToken = OAuthToken T.Text

data Channel = Channel T.Text

data MessagePayload = MessagePayload
    { channel :: T.Text
    , text :: T.Text
    }
    deriving (Show, Eq, Ord, Generic)

encodingOptions :: A.Options
encodingOptions =
    A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' }

instance A.ToJSON MessagePayload where
    toEncoding =
        A.genericToEncoding encodingOptions
