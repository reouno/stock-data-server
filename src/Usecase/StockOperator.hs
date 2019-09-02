{-# LANGUAGE DeriveGeneric #-}

module Usecase.StockOperator
  ( Condition(..)
  , StockStorable(..)
  , PriceType(..)
  , Stock(..)
  , StockId
  ) where

import           Data.Aeson   ( FromJSON, ToJSON )
import           Data.Time    ( Day )
import           Entity.Stock ( PriceType (..), Stock (..), StockId )
import           GHC.Generics

-- for stock server
data Condition =
  Condition
    { condPriceType :: PriceType
    , condStartTime :: Day
    , condEndTime   :: Day
    }
  deriving (Eq, Generic, Show)

instance FromJSON Condition

instance ToJSON Condition

class StockStorable storage where
  get :: storage -> StockId -> Stock -- get all data of the stock ID
  getBy :: storage -> Condition -> StockId -> Stock -- get data in specific condition
  add :: storage -> Stock -> IO (Either String StockId) -- add new stock data
  update :: storage -> Stock -> StockId -> IO (Either String StockId) -- update stock data
  delete :: storage -> StockId -> IO (Either String StockId) -- delete stock data
