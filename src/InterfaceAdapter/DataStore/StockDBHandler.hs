module InterfaceAdapter.DataStore.StockDBHandler
  ( getStockInfo
  , getStockInfos
  , insertStockInfo
  , getStockPrice
  , getStockPrices
  , insertStockPrice
  ) where

import           Database.Persist.Sql
import           InterfaceAdapter.DataStore.StockDBModel (StockInfo (..),
                                                          StockPrice (..),
                                                          emptyStockInfo,
                                                          migrateAll)

getStockInfo :: ConnectionPool -> Key StockInfo -> IO (Maybe StockInfo)
getStockInfo pool id' = flip runSqlPool pool $ get id'

getStockInfos :: ConnectionPool -> IO [Entity StockInfo]
getStockInfos pool = flip runSqlPool pool $ selectList [] []

insertStockInfo :: ConnectionPool -> StockInfo -> IO (Key StockInfo)
insertStockInfo pool stockInfo = flip runSqlPool pool $ insert stockInfo

getStockPrice :: ConnectionPool -> Key StockPrice -> IO (Maybe StockPrice)
getStockPrice pool id' = flip runSqlPool pool $ get id'

getStockPrices :: ConnectionPool -> IO [Entity StockPrice]
getStockPrices pool = flip runSqlPool pool $ selectList [] []

insertStockPrice :: ConnectionPool -> StockPrice -> IO (Key StockPrice)
insertStockPrice pool stockPrice = flip runSqlPool pool $ insert stockPrice
