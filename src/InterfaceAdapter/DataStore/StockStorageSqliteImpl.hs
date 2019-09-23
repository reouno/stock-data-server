{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module InterfaceAdapter.DataStore.StockStorageSqliteImpl
  ( ConnPool(..)
  -- TODO: remove the following exports
  , getStockInfo
  , getStockInfos
  , insertStockInfo
  , getStockPrice
  , getStockPrices
  , insertStockPrice
  ) where

import           Basement.IntegralConv                     ( intToInt64 )
import           Control.Monad.Logger                      ( runStderrLoggingT )
import           Data.String.Conversions                   ( cs )
import           Data.Time                                 ( getCurrentTime )
import           Database.Persist.Sqlite

import           InterfaceAdapter.DataStore.StockDBHandler ( getStockInfo, getStockInfos,
                                                             getStockPrice, getStockPrices,
                                                             insertStockInfo, insertStockPrice )
import           InterfaceAdapter.DataStore.StockDBModel   ( StockInfo (..), StockPrice (..),
                                                             emptyStockInfo, migrateAll )
import           InterfaceAdapter.StockDataAdapter         ( fromStockToStockInfo,
                                                             fromStockToStockPrices, toStock )
import           Usecase.Interface.StockStorage            ( StockStorage (..) )

type ConnPool = ConnectionPool

instance StockStorage ConnPool where
  mkPool :: FilePath -> IO ConnPool
  mkPool filePath = runStderrLoggingT $ createSqlitePool (cs filePath) 5
  initialize :: ConnPool -> IO ()
  initialize = runSqlPool (runMigration migrateAll)
  --addStockEntity :: ConnPool -> Stock -> IO StockId
  addStockEntity pool stock = do
    now <- getCurrentTime
    let stockInfo = fromStockToStockInfo stock
    stockInfoKey <- insertStockInfo pool stockInfo
    let stockId = fromIntegral $ fromSqlKey stockInfoKey :: Int
        stockPrices = fromStockToStockPrices stock stockId now
    mapM_ (insertStockPrice pool) stockPrices
    return stockId
  --getStockEntity :: ConnPool -> StockId -> IO Stock
  getStockEntity pool stockId = do
    let stockInfoId = (toSqlKey . intToInt64) stockId
    mayStockInfo <- getStockInfo pool stockInfoId
    thisStockInfo <-
      case mayStockInfo of
        Just info -> return info
        Nothing   -> return emptyStockInfo
    allPrices <- getStockPrices pool
    let thisStockPrices =
          [ x
          | x <- allPrices
          , stockPriceStockId (entityVal x) ==
              (fromIntegral . fromSqlKey) stockInfoId
          ] -- only Apple stock data
        thisStockData = toStock thisStockInfo (map entityVal thisStockPrices)
    return thisStockData
