module Usecase.Interactor.PlainStockServer
  ( serveStockFromStore
  , addStockToStore
  , deleteStockByStockId
  ) where

import           Usecase.Interface.PresentableData ( StockIdPresentable (..),
                                                     StockPresentable (..) )
import           Usecase.Interface.StockStorage    ( StockStorage (..) )

serveStockFromStore ::
     (StockStorage pool, StockPresentable stock, StockIdPresentable stockId)
  => pool
  -> stockId
  -> IO stock
serveStockFromStore pool id' =
  toPresentableStock <$> getStockEntity pool (fromPresentableStockId id')

addStockToStore ::
     (StockStorage pool, StockPresentable stock, StockIdPresentable stockId)
  => pool
  -> stock
  -> IO stockId
addStockToStore pool presentableStock =
  toPresentableStockId <$>
  addStockEntity pool (fromPresentableStock presentableStock)

deleteStockByStockId ::
     (StockStorage pool, StockIdPresentable stockId) => pool -> stockId -> IO ()
deleteStockByStockId pool stockId =
  deleteStockEntity pool $ fromPresentableStockId stockId
