module InterfaceAdapter.StockDataAdapter
  ( toStock
  ) where

import           Data.Time                               (Day)
import           Entity.Stock                            (PriceType (..),
                                                          Stock (..),
                                                          StockPrice (..))
import qualified InterfaceAdapter.DataStore.StockDBModel as CSM
import           Numeric.Extra                           (doubleToFloat)

toStock :: CSM.StockInfo -> [CSM.StockPrice] -> Stock
toStock info prices =
  Stock
    (CSM.stockInfoName info)
    (CSM.stockInfoTickerSymbol info)
    D1
    stockPrices
    dates
  where
    toStockPrice :: CSM.StockPrice -> StockPrice
    toStockPrice csmPrice =
      StockPrice
        (Just $ doubleToFloat (CSM.stockPriceOpenPrice csmPrice))
        (doubleToFloat (CSM.stockPriceClosePrice csmPrice))
        (Just $ doubleToFloat (CSM.stockPriceHighPrice csmPrice))
        (Just $ doubleToFloat (CSM.stockPriceLowPrice csmPrice))
    dates = map CSM.stockPricePriceDate prices :: [Day]
    stockPrices = map toStockPrice prices :: [StockPrice]
