module Usecase.Interface.StorableData
  ( StorableStockInfo(..)
  , StorableStockPrice(..)
  ) where

import           Data.Time     ( Day, UTCTime )
import           Entity.Stock  ( PriceType (..), Stock (..), StockId, StockPrice (..) )
import           Numeric.Extra ( doubleToFloat, floatToDouble )

class StorableStockInfo stockInfo where
  toStorableStockInfo :: String -> String -> stockInfo
  fromStorableStockInfo :: stockInfo -> (String, String)

class StorableStockPrice stockPrice where
  toStorableStockPrice ::
       Int
    -> Maybe Double
    -> Double
    -> Maybe Double
    -> Maybe Double
    -> Day
    -> UTCTime
    -> stockPrice
  fromStorableStockPrice ::
       stockPrice
    -> (Int, Maybe Double, Double, Maybe Double, Maybe Double, Day, UTCTime)
