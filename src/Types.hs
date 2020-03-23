{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Aeson as A
-- import qualified Data.List as List
import qualified Data.Text as Tx
import GHC.Generics (Generic)
-- import Text.Read (readMaybe)

data Config = Config
    { slackChannelId :: Tx.Text
    , slackOauthToken :: Tx.Text
    }

data OrderBody = OrderBody
    { email :: Tx.Text
    , address :: Tx.Text
    , amount :: Int
    , deliveryTime :: Tx.Text
    }
    deriving (Show, Eq, Ord, Generic)

customOptions =
    A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' }

instance A.FromJSON OrderBody where
    parseJSON = A.genericParseJSON customOptions
instance A.ToJSON OrderBody where
    toEncoding = A.genericToEncoding customOptions

data SlackMessagePayload = SlackMessagePayload
    { channel :: Tx.Text
    , text :: Tx.Text
    }
    deriving (Show, Eq, Ord, Generic)

instance A.ToJSON SlackMessagePayload where
    toEncoding = A.genericToEncoding customOptions

parseConfig :: Maybe String -> Maybe String -> Either Tx.Text Config
parseConfig maybe_channel_id maybe_oauth_token = case maybe_channel_id of
    Nothing -> Left "Missing environment variable: HASCOOKIE_SLACK_CHANNEL_ID"
    Just channel_id -> case maybe_oauth_token of
        Nothing -> Left "Missing environment variable: HASCOOKIE_SLACK_TOKEN"
        Just oauth_token ->
            Right $ Config (Tx.pack channel_id) (Tx.pack oauth_token)

-- data MessageBlock =
--   TextBlock Tx.Text
--   | ActionsBlock [MessageBlock]
--   | ButtonBlock Tx.Text

-- parseOrderBody :: [(Tx.Text, T.Text)] -> Maybe OrderBody
-- parseOrderBody params = case List.sortOn fst params of
--   [("amount", amount), ("deliveryTime", deliveryTime), ("email", email), ("address", address)]
--     -> do
--       parsedAmount <- readMaybe $ Tx.unpack amount
--       return $ OrderBody email address parsedAmount deliveryTime
--   _ -> Nothing
