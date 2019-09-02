module Controller.StockStorage
  ( dummyStock
  ) where

import           Control.Monad.Logger    ( runStderrLoggingT )
import           Controller.StockModel   ( migrateAll )
import           Data.String.Conversions ( cs )
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           Usecase.StockOperator   ( PriceType (..), Stock (..) )

dummyStock = Stock "Apple" "NASDAQ:AAPL" Close [] []

type ConnPool = ConnectionPool

mkPool :: FilePath -> IO ConnPool
mkPool filePath = runStderrLoggingT $ createSqlitePool (cs filePath) 5

doMigration :: ConnPool -> IO ()
doMigration = runSqlPool (runMigration migrateAll)
