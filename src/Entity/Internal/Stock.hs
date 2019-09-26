{-# LANGUAGE DeriveGeneric #-}

module Entity.Internal.Stock
  ( PriceType(..)
  , Stock(..)
  , StockPrice(..)
  , StockId
  ) where

import           Data.Aeson   ( FromJSON, ToJSON )
import           Data.Time    ( Day )
import           GHC.Generics ( Generic )

type StockId = Int

type StockName = String

type TickerSymbol = String

data StockPrice =
  StockPrice
    { stockPriceOpenPrice  :: Maybe Float
    , stockPriceClosePrice :: Float
    , stockPriceHighPrice  :: Maybe Float
    , stockPriceLowPrice   :: Maybe Float
    }
  deriving (Eq, Generic, Show)

instance FromJSON StockPrice

instance ToJSON StockPrice

-- The minimum unit is Daily (D1)
data PriceType
  = D1
  | W1
  | M1
  | M3
  | M6
  | Y1
  deriving (Eq, Generic, Show)

instance FromJSON PriceType

instance ToJSON PriceType

data Stock =
  Stock
    { stockEntName            :: StockName
    , stockEntTickerSymbol    :: TickerSymbol
    , stocKEntPriceType       :: PriceType
    , stockEntPrices          :: [StockPrice]
    , stockEntPriceTimestamps :: [Day]
    }
  deriving (Eq, Generic, Show)

instance FromJSON Stock

instance ToJSON Stock
