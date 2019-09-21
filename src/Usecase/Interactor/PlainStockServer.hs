module Usecase.Interactor.PlainStockServer
  ( serveStockFromStore
  ) where

import           Usecase.Interface.PresentableData (StockIdPresentable (..),
                                                    StockPresentable (..))
import           Usecase.Interface.StockStorage    (StockStorage (..))

serveStockFromStore ::
     (StockStorage pool, StockPresentable stock, StockIdPresentable stockId)
  => pool
  -> stockId
  -> IO stock
serveStockFromStore pool id' =
  toPresentableStock <$> getStockEntity pool (fromPresentableStockId id')
