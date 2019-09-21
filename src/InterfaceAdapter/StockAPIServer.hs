{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module InterfaceAdapter.StockAPIServer
  ( app
  ) where

-- TODO: Do NOT import Config here!! Config should be imported only in Main.
import           Config.Config                                     (_database_)
import           Control.Monad                                     (when)
import           InterfaceAdapter.DataStore.StockDBHandler         (insertStockInfo,
                                                                    insertStockPrice)
import           InterfaceAdapter.DataStore.StockDBModel           (sampleStockInfo1,
                                                                    sampleStockInfo2,
                                                                    sampleStockPrice1,
                                                                    sampleStockPrice2,
                                                                    sampleStockPrice3)
import           InterfaceAdapter.DataStore.StockStorageSqliteImpl (ConnPool)
import           InterfaceAdapter.Presenter.PresentableDataImpl    (Stock',
                                                                    StockId')
import           InterfaceAdapter.Presenter.StockAPIHandler        (StockAPI (..),
                                                                    stockAPI,
                                                                    stockServer)
import           Servant
import           System.Directory
import           Usecase.Interface.StockStorage                    (StockStorage (..))
import           Usecase.StockCondition                            (Condition)

import           Database.Persist.Sql
import           Database.Persist.Sqlite

server :: IO (Server (StockAPI Stock' StockId' Condition))
server = do
  fileExists <- doesFileExist _database_
  when fileExists $ removeFile _database_
  pool <- mkPool _database_
  initialize pool
  -- experiments start
  iId1 <- insertStockInfo pool sampleStockInfo1
  iId2 <- insertStockInfo pool sampleStockInfo2
  pId1 <- insertStockPrice pool sampleStockPrice1
  pId2 <- insertStockPrice pool sampleStockPrice2
  pId3 <- insertStockPrice pool sampleStockPrice3
  let server' = stockServer pool
  return server'

app :: IO Application
app = serve stockAPI <$> server
