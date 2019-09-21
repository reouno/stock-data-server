{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Controller.StockAPIHandler
  ( StockAPI(..)
  , stockAPI
  , stockServer
  ) where

import           Control.Monad.IO.Class              (liftIO)
import           Controller.PresentableDataImpl      (Stock' (..), StockId')
import           Controller.StockDatabase            (ConnPool)
import           Servant
import           Usecase.Interactor.PlainStockServer (serveStockFromStore)
import           Usecase.StockCondition              (Condition)

--class PersistentDataStore pool where
type StockAPI
   = Get '[ JSON] String -- for test
      :<|> ReqBody '[ JSON] Stock' :> Post '[ JSON] String -- add stock data
      :<|> Capture "stockId" StockId' :> (Get '[ JSON] Stock' -- get stock data
                                           :<|> ReqBody '[ JSON] Condition :> Post '[ JSON] Stock' -- get stock data by specific condition
                                           :<|> ReqBody '[ JSON] Stock' :> Put '[ JSON] String -- update stock data
                                           :<|> Delete '[ JSON] String -- delete stock data
                                          )

stockAPI :: Proxy StockAPI
stockAPI = Proxy

stockServer :: ConnPool -> Server StockAPI
stockServer pool = testGet :<|> addStock :<|> stockOperations pool
  where
    testGet :: Handler String
    testGet = return "Hi, I'm Stock API server."
    addStock :: Stock' -> Handler String
    addStock stock = return "Cannot add new Stock, please implement me!"
    stockOperations pool stockId =
      getStock pool stockId :<|> getStockBy stockId :<|> updateStock stockId :<|>
      deleteStock stockId
      where
        getStock :: ConnPool -> StockId' -> Handler Stock'
        getStock pool stockId = liftIO $ serveStockFromStore pool stockId
        getStockBy :: StockId' -> Condition -> Handler Stock'
        getStockBy stockId condition =
          error "`getStockBy` was called. Not implemented yet."
        updateStock :: StockId' -> Stock' -> Handler String
        updateStock stockId stock =
          return "Cannot update Stock, please implement me!"
        deleteStock :: StockId' -> Handler String
        deleteStock stockId = return "Cannot detele Stock, please implement me!"
