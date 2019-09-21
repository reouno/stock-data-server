{-# LANGUAGE DeriveGeneric #-}

module Usecase.StockCondition
  ( Condition(..)
  ) where

import           Data.Aeson   ( FromJSON, ToJSON )
import           Data.Time    ( Day )
import           Entity.Stock ( PriceType (..), Stock (..), StockId )
import           GHC.Generics ( Generic )

-- for stock server
data Condition =
  Condition
    { condPriceType :: PriceType
    , condStartTime :: Day
    , condEndTime   :: Day
    }
  deriving (Eq, Generic, Show)

instance FromJSON Condition

instance ToJSON Condition
