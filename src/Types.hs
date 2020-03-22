{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Aeson                    as A
import qualified Data.Text                     as T
import qualified Data.List                     as List
import           Text.Read                      ( readMaybe )
import           GHC.Generics                   ( Generic )

data Config = Config
  { slackChannelId :: T.Text
  , slackOauthToken :: T.Text
  }

data OrderBody = OrderBody
  { email :: T.Text
  , address :: T.Text
  , amount :: Int
  , deliveryTime :: T.Text
  }
  deriving (Show, Eq, Ord, Generic)

customOptions = A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_' }

instance A.FromJSON OrderBody where
  parseJSON = A.genericParseJSON customOptions
instance A.ToJSON OrderBody where
  toEncoding = A.genericToEncoding customOptions

data SlackMessagePayload = SlackMessagePayload
  { channel :: T.Text
  , text :: T.Text
  }
  deriving (Show, Eq, Ord, Generic)

instance A.ToJSON SlackMessagePayload where
  toEncoding = A.genericToEncoding customOptions

data MessageBlock =
  TextBlock T.Text
  | ActionsBlock [MessageBlock]
  | ButtonBlock T.Text

parseConfig :: Maybe String -> Maybe String -> Either T.Text Config
parseConfig maybe_channel_id maybe_oauth_token = case maybe_channel_id of
  Nothing -> Left "Missing environment variable: HASCOOKIE_SLACK_CHANNEL_ID"
  Just channel_id -> case maybe_oauth_token of
    Nothing -> Left "Missing environment variable: HASCOOKIE_SLACK_TOKEN"
    Just oauth_token -> Right $ Config (T.pack channel_id) (T.pack oauth_token)

-- parseOrderBody :: [(T.Text, T.Text)] -> Maybe OrderBody
-- parseOrderBody params = case List.sortOn fst params of
--   [("amount", amount), ("deliveryTime", deliveryTime), ("email", email), ("address", address)]
--     -> do
--       parsedAmount <- readMaybe $ T.unpack amount
--       return $ OrderBody email address parsedAmount deliveryTime
--   _ -> Nothing
