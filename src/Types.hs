{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Aeson as A
-- import qualified Data.List as List
import qualified Data.Text as T
import GHC.Generics (Generic)
-- import Text.Read (readMaybe)

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

-- data MessageBlock
--     = TextBlock T.Text
--     | ActionsBlock [MessageBlock]
--     | ButtonBlock T.Text


-- parseOrderBody :: [(T.Text, T.Text)] -> Maybe OrderBody
-- parseOrderBody params =
--   case List.sortOn fst params of
--     [("amount", amount),
--      ("deliveryTime", deliveryTime),
--      ("email", email), ("address", address)] ->
--       do
--         parsedAmount <- readMaybe $ T.unpack amount
--         return $ OrderBody email address parsedAmount deliveryTime
--     _ ->
--       Nothing
