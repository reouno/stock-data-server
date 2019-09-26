{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module InterfaceAdapter.DataStore.StockDBModel where

import           Data.Time                      ( Day, UTCTime, fromGregorian )
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics                   ( Generic )
import           Usecase.Interface.StorableData ( StorableStockInfo (..), StorableStockPrice (..) )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
StockInfo json
    name String
    tickerSymbol String
    deriving Read Eq Generic Show
StockPrice json
    stockId Int
    openPrice Double Maybe
    closePrice Double
    highPrice Double Maybe
    lowPrice Double Maybe
    priceDate Day
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Read Eq Generic Show
|]

instance StorableStockInfo StockInfo where
  toStorableStockInfo :: String -> String -> StockInfo
  toStorableStockInfo = StockInfo
  fromStorableStockInfo :: StockInfo -> (String, String)
  fromStorableStockInfo (StockInfo name tickerSymbol) = (name, tickerSymbol)

instance StorableStockPrice StockPrice where
  toStorableStockPrice = StockPrice
  fromStorableStockPrice (StockPrice sId op cp hp lp pd cAt) =
    (sId, op, cp, hp, lp, pd, cAt)

emptyStockInfo = StockInfo "" ""

sampleStockInfo1 = StockInfo "Apple, Inc." "AAPL"

sampleStockInfo2 = StockInfo "Alphabet, Inc." "GOOG"

sampleStockPrice1 =
  StockPrice
    1
    (Just 220.00)
    223.09
    (Just 220.79)
    (Just 217.02)
    (fromGregorian 2019 09 13)
    (read "2019-09-16 14:00:00.012345 UTC" :: UTCTime)

sampleStockPrice2 =
  StockPrice
    2
    (Just 1231.35)
    1234.25
    (Just 1240.88)
    (Just 1227.01)
    (fromGregorian 2019 09 13)
    (read "2019-09-16 15:00:00.0 UTC" :: UTCTime)

sampleStockPrice3 =
  StockPrice
    1
    (Just 230.00)
    233.09
    (Just 230.79)
    (Just 227.02)
    (fromGregorian 2019 09 12)
    (read "2019-09-16 16:00:00.5 UTC" :: UTCTime)
