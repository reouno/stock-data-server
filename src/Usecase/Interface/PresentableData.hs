module Usecase.Interface.PresentableData
  ( StockPresentable(..)
  , StockIdPresentable(..)
  ) where

import           Entity.Stock (Stock, StockId)

class StockPresentable stock where
  toPresentableStock :: Stock -> stock
  fromPresentableStock :: stock -> Stock

class StockIdPresentable stockId where
  toPresentableStockId :: StockId -> stockId
  fromPresentableStockId :: stockId -> StockId
