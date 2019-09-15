module Usecase.PreviousDayStockServer () where

import qualified Data.Map as M
import Entity.Stock (StockId, Stock(..))


{-
 - Decide the specification first.
 - This Usecase serve methods to get previous day's stock data.
 - Idea 1: serve simply the getAll method
 - Idea 2: serve the getBy method that takes stock id list
 - Idea 3: serve both getAll and getBy methods for generality.
-}
--getAllPreviousDayStocks :: StockStore storage => storage -> M.Map StockId Stock
--getAllPreviousDayStocks srg = 
