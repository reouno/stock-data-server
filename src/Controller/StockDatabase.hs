{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module Controller.StockDatabase where

import           Basement.IntegralConv          (intToInt64)
import           Control.Monad.Logger           (runStderrLoggingT)
import           Data.String.Conversions        (cs)
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Controller.StockDataAdapter    (toStock)
import           Controller.StockModel          (StockInfo (..),
                                                 StockPrice (..),
                                                 emptyStockInfo, migrateAll)
import           Usecase.Interface.StockStorage (StockStorage (..))

-- TODO: How to remove this dependency?
import           Entity.Stock                   (PriceType (..), Stock (..),
                                                 StockId)

--instance Source FilePath
type ConnPool = ConnectionPool

instance StockStorage ConnPool where
  mkPool :: FilePath -> IO ConnPool
  mkPool filePath = runStderrLoggingT $ createSqlitePool (cs filePath) 5
  initialize :: ConnPool -> IO ()
  initialize = runSqlPool (runMigration migrateAll)
  getStockEntity :: ConnPool -> StockId -> IO Stock
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

getStockInfo :: ConnPool -> Key StockInfo -> IO (Maybe StockInfo)
getStockInfo pool id' = flip runSqlPool pool $ get id'

getStockInfos :: ConnPool -> IO [Entity StockInfo]
getStockInfos pool = flip runSqlPool pool $ selectList [] []

insertStockInfo :: ConnPool -> StockInfo -> IO (Key StockInfo)
insertStockInfo pool stockInfo = flip runSqlPool pool $ insert stockInfo

getStockPrice :: ConnPool -> Key StockPrice -> IO (Maybe StockPrice)
getStockPrice pool id' = flip runSqlPool pool $ get id'

getStockPrices :: ConnPool -> IO [Entity StockPrice]
getStockPrices pool = flip runSqlPool pool $ selectList [] []

insertStockPrice :: ConnPool -> StockPrice -> IO (Key StockPrice)
insertStockPrice pool stockPrice = flip runSqlPool pool $ insert stockPrice
