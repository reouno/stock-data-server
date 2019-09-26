module Usecase.Interactor.Adapter.StockDataAdapter
  ( toStock
  , fromStockToStorableStockInfo
  , fromStockToStorableStockPrices
  ) where

import           Data.Time                      ( Day, UTCTime )
import           Entity.Stock                   ( PriceType (..), Stock (..), StockId,
                                                  StockPrice (..) )
import           Numeric.Extra                  ( doubleToFloat, floatToDouble )
import           Usecase.Interface.StorableData ( StorableStockInfo (..), StorableStockPrice (..) )

fromStockToStorableStockInfo :: StorableStockInfo si => Stock -> si
fromStockToStorableStockInfo stock =
  toStorableStockInfo (stockEntName stock) (stockEntTickerSymbol stock)

fromStockToStorableStockPrices ::
     StorableStockPrice sp => Stock -> StockId -> UTCTime -> [sp]
fromStockToStorableStockPrices stock stockId now =
  zipWith toStockPrice (stockEntPrices stock) (stockEntPriceTimestamps stock)
  where
    toStockPrice stockPrice priceDay =
      toStorableStockPrice
        stockId
        (floatToDouble <$> stockPriceOpenPrice stockPrice)
        (floatToDouble $ stockPriceClosePrice stockPrice)
        (floatToDouble <$> stockPriceHighPrice stockPrice)
        (floatToDouble <$> stockPriceLowPrice stockPrice)
        priceDay
        now

toStockPrice :: StorableStockPrice sp => sp -> StockPrice
toStockPrice sp =
  StockPrice
    (doubleToFloat <$> op)
    (doubleToFloat cp)
    (doubleToFloat <$> hp)
    (doubleToFloat <$> lp)
  where
    (_, op, cp, hp, lp, _, _) = fromStorableStockPrice sp

toStock :: (StorableStockInfo si, StorableStockPrice sp) => si -> [sp] -> Stock
toStock info prices = Stock name tickerSymbol D1 stockPrices dates
  where
    (name, tickerSymbol) = fromStorableStockInfo info
    getDate ps = date
      where
        (_, _, _, _, _, date, _) = fromStorableStockPrice ps
    dates = map getDate prices :: [Day]
    stockPrices = map toStockPrice prices :: [StockPrice]
