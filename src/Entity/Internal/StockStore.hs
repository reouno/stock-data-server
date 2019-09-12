module Entity.Internal.StockStore
  ( StockStore(..)
  ) where

import qualified Data.Map              as M
import           Data.Time             ( Day )
import           Entity.Internal.Stock ( PriceType, Stock (..), StockId )

class StockStore storage where
  create :: storage -> Stock -> IO (Either String StockId)
  read :: storage -> StockId -> IO (Either String Stock)
  readAll :: storage -> M.Map StockId Stock
  readBy :: storage -> PriceType -> Day -> Day -> StockId -> Stock
  update :: storage -> Stock -> StockId -> IO (Either String StockId)
