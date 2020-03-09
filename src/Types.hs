{-# LANGUAGE OverloadedStrings #-}
module Types where

-- import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.List as List
import Text.Read (readMaybe)

data OrderBody = OrderBody { email :: T.Text
                 , plz :: T.Text
                 , amount :: Int
                 , deliveryTime :: T.Text
                 }
  deriving (Show, Eq, Ord)


data MessageBlock =
  TextBlock T.Text
  | ActionsBlock [MessageBlock]
  | ButtonBlock T.Text


parseOrderBody :: [(T.Text, T.Text)] -> Maybe OrderBody
parseOrderBody params =
  case List.sortOn fst params of
    [("amount", amount),
     ("deliveryTime", deliveryTime),
     ("email", email), ("plz", plz)] ->
      do
        parsedAmount <- readMaybe $ T.unpack amount
        return $ OrderBody email plz parsedAmount deliveryTime
    _ ->
      Nothing
