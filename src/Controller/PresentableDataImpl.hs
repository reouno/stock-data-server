{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Controller.PresentableDataImpl
  ( Stock'(..)
  , StockId'(..)
  ) where

import           Data.Char
import           Usecase.Interface.PresentableData (Stock (..), StockId,
                                                    StockIdPresentable (..),
                                                    StockPresentable (..))

type Stock' = Stock

type StockId' = StockId

instance StockPresentable Stock' where
  toPresentableStock :: Stock -> Stock'
  toPresentableStock = id
  fromPresentableStock :: Stock' -> Stock
  fromPresentableStock = id

instance StockIdPresentable StockId' where
  toPresentableStockId :: StockId -> StockId'
  toPresentableStockId = id
  fromPresentableStockId :: StockId' -> StockId
  fromPresentableStockId = id
