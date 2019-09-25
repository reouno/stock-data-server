module InterfaceAdapter.StockDataAdapter
  ( toStock
  , fromStockToStockInfo
  , fromStockToStockPrices
  ) where

import           Data.Time                               ( Day, UTCTime )
import           Entity.Stock                            ( PriceType (..), Stock (..), StockId,
                                                           StockPrice (..) )
import qualified InterfaceAdapter.DataStore.StockDBModel as CSM
import           Numeric.Extra                           ( doubleToFloat, floatToDouble )

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
        (doubleToFloat <$> CSM.stockPriceOpenPrice csmPrice)
        (doubleToFloat (CSM.stockPriceClosePrice csmPrice))
        (doubleToFloat <$> CSM.stockPriceHighPrice csmPrice)
        (doubleToFloat <$> CSM.stockPriceLowPrice csmPrice)
    dates = map CSM.stockPricePriceDate prices :: [Day]
    stockPrices = map toStockPrice prices :: [StockPrice]

fromStockToStockInfo :: Stock -> CSM.StockInfo
fromStockToStockInfo stock =
  CSM.StockInfo (stockName stock) (tickerSymbol stock)

fromStockToStockPrices :: Stock -> StockId -> UTCTime -> [CSM.StockPrice]
fromStockToStockPrices stock sId now =
  zipWith toStockPrice (prices stock) (priceTimestamps stock)
  where
    toStockPrice stockPrice priceDay =
      CSM.StockPrice
        sId
        (floatToDouble <$> openPrice stockPrice)
        (floatToDouble $ closePrice stockPrice)
        (floatToDouble <$> highPrice stockPrice)
        (floatToDouble <$> lowPrice stockPrice)
        priceDay
        now
