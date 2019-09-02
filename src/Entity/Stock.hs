{-# LANGUAGE DeriveGeneric #-}

module Entity.Stock
  ( PriceType(..)
  , Stock(..)
  , StockId
  ) where

import           Data.Aeson   ( FromJSON, ToJSON )
import           Data.Time    ( UTCTime )
import           GHC.Generics ( Generic )

type StockId = Int

type StockName = String

type TickerSymbol = String

data PriceType
  = Open
  | Close
  | High
  | Low
  | Mi1
  | Mi15
  | H1
  | H8
  | W1
  | Mo1
  | Y1
  deriving (Eq, Generic, Show)

instance FromJSON PriceType

instance ToJSON PriceType

data Stock =
  Stock
    { stockName       :: StockName
    , tickerSymbol    :: TickerSymbol
    , priceType       :: PriceType
    , prices          :: [Float]
    , priceTimestamps :: [UTCTime]
    }
  deriving (Eq, Generic, Show)

instance FromJSON Stock

instance ToJSON Stock
