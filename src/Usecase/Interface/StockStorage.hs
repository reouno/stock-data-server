module Usecase.Interface.StockStorage
  ( dummyStock
  , StockStorage(..)
  ) where

import           Entity.Stock (PriceType (..), Stock (..), StockId)

dummyStock = Stock "Apple" "NASDAQ:AAPL" D1 [] []

--class Source s
class StockStorage pool where
  mkPool :: FilePath -> IO pool
  initialize :: pool -> IO ()
  getStockEntity :: pool -> StockId -> IO Stock -- get all data of the stock ID
