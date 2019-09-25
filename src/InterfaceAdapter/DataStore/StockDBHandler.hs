module InterfaceAdapter.DataStore.StockDBHandler
  ( getStockInfo
  , getStockInfos
  , insertStockInfo
  , deleteStockInfo
  , getStockPrice
  , getStockPrices
  , insertStockPrice
  , deleteStockPrice
  , deleteStockPrices
  ) where

import           Database.Persist.Sql
import           InterfaceAdapter.DataStore.StockDBModel

getStockInfo :: ConnectionPool -> Key StockInfo -> IO (Maybe StockInfo)
getStockInfo pool id' = flip runSqlPool pool $ get id'

getStockInfos :: ConnectionPool -> IO [Entity StockInfo]
getStockInfos pool = flip runSqlPool pool $ selectList [] []

insertStockInfo :: ConnectionPool -> StockInfo -> IO (Key StockInfo)
insertStockInfo pool stockInfo = flip runSqlPool pool $ insert stockInfo

deleteStockInfo :: ConnectionPool -> Key StockInfo -> IO ()
deleteStockInfo pool id' = flip runSqlPool pool $ delete id'

getStockPrice :: ConnectionPool -> Key StockPrice -> IO (Maybe StockPrice)
getStockPrice pool id' = flip runSqlPool pool $ get id'

getStockPrices :: ConnectionPool -> IO [Entity StockPrice]
getStockPrices pool = flip runSqlPool pool $ selectList [] []

insertStockPrice :: ConnectionPool -> StockPrice -> IO (Key StockPrice)
insertStockPrice pool stockPrice = flip runSqlPool pool $ insert stockPrice

deleteStockPrice :: ConnectionPool -> Key StockPrice -> IO ()
deleteStockPrice pool id' = flip runSqlPool pool $ delete id'

deleteStockPrices :: ConnectionPool -> Key StockInfo -> IO ()
deleteStockPrices pool infoId =
  flip runSqlPool pool $
  deleteWhere [StockPriceStockId ==. (fromIntegral . fromSqlKey) infoId]
