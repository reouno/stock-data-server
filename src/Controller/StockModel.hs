{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Controller.StockModel
  ( migrateAll
  ) where

import           Data.Time            ( Day, UTCTime )
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics         ( Generic )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Stock json
    name String
    tickerSymbol String
    openPrice Double
    closePrice Double
    hightPrice Double
    lowPrice Double
    priceDate Day
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Read Eq Generic Show
|]
