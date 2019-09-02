{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Controller.StockServer
  ( app
  ) where

import           Controller.StockStorage ( dummyStock )
import           Servant
import           Usecase.StockOperator   ( Condition, Stock, StockId, StockStorable (..) )

type StockAPI
   = Get '[ JSON] String -- for test
      :<|> ReqBody '[ JSON] Stock :> Post '[ JSON] String -- add stock data
      :<|> Capture "stockId" Int :> (Get '[ JSON] Stock -- get stock data
                                      :<|> ReqBody '[ JSON] Condition :> Post '[ JSON] Stock -- get stock data by specific condition
                                      :<|> ReqBody '[ JSON] Stock :> Put '[ JSON] String -- update stock data
                                      :<|> Delete '[ JSON] String -- delete stock data
                                     )

stockAPI :: Proxy StockAPI
stockAPI = Proxy

stockServer :: Server StockAPI
stockServer = testGet :<|> addStock :<|> stockOperations
  where
    testGet :: Handler String
    testGet = return "Hi, I'm Stock API server."
    addStock :: Stock -> Handler String
    addStock stock = return "Cannot add new Stock, please implement me!"
    stockOperations stockId =
      getStock stockId :<|> getStockBy stockId :<|> updateStock stockId :<|>
      deleteStock stockId
      where
        getStock :: StockId -> Handler Stock
        getStock stockId = return dummyStock
        getStockBy :: StockId -> Condition -> Handler Stock
        getStockBy stockId condition = return dummyStock
        updateStock :: StockId -> Stock -> Handler String
        updateStock stockId stock =
          return "Cannot update Stock, please implement me!"
        deleteStock :: StockId -> Handler String
        deleteStock stockId = return "Cannot detele Stock, please implement me!"

app :: Application
app = serve stockAPI stockServer
