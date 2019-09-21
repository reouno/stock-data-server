module Usecase.Interface.PresentableData
  ( StockPresentable(..)
  , StockIdPresentable(..)
  -- TODO: Is it really acceptable reexporting Entity data types here?
  -- for PresentableDataImpl
  , Stock(..)
  , StockId
  ) where

import           Entity.Stock (Stock (..), StockId)

class StockPresentable stock where
  toPresentableStock :: Stock -> stock
  fromPresentableStock :: stock -> Stock

class StockIdPresentable stockId where
  toPresentableStockId :: StockId -> stockId
  fromPresentableStockId :: stockId -> StockId
