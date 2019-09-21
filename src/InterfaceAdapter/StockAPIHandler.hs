{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module InterfaceAdapter.StockAPIHandler
  ( StockAPI(..)
  , stockAPI
  , stockServer
  ) where

import           Control.Monad.IO.Class              (liftIO)
import           Servant
import           Usecase.Interactor.PlainStockServer (serveStockFromStore)
import           Usecase.Interface.PresentableData   (StockIdPresentable (..),
                                                      StockPresentable (..))
import           Usecase.Interface.StockStorage      (StockStorage (..))

type StockAPI stock stockId condition
   = Get '[ JSON] String -- for test
      :<|> ReqBody '[ JSON] stock :> Post '[ JSON] String -- add stock data
      :<|> Capture "stockId" stockId :> (Get '[ JSON] stock -- get stock data
                                          :<|> ReqBody '[ JSON] condition :> Post '[ JSON] stock -- get stock data by specific condition
                                          :<|> ReqBody '[ JSON] stock :> Put '[ JSON] String -- update stock data
                                          :<|> Delete '[ JSON] String -- delete stock data
                                         )

stockAPI :: Proxy (StockAPI stock stockId condition)
stockAPI = Proxy

stockServer ::
     (StockStorage pool, StockPresentable stock, StockIdPresentable stockId)
  => pool
  -> Server (StockAPI stock stockId condition)
stockServer pool = testGet :<|> addStock pool :<|> stockOperations pool
  where
    testGet :: Handler String
    testGet = return "Hi, I'm Stock API server."
    addStock ::
         (StockStorage pool, StockPresentable stock)
      => pool
      -> stock
      -> Handler String
    addStock pool stock = return "Cannot add new Stock, please implement me!"
    stockOperations pool stockId =
      getStock pool stockId :<|> getStockBy pool stockId :<|>
      updateStock pool stockId :<|>
      deleteStock pool stockId
      where
        getStock ::
             ( StockStorage pool
             , StockPresentable stock
             , StockIdPresentable stockId
             )
          => pool
          -> stockId
          -> Handler stock
        getStock pool stockId = liftIO $ serveStockFromStore pool stockId
        getStockBy ::
             ( StockStorage pool
             , StockPresentable stock
             , StockIdPresentable stockId
             )
          => pool
          -> stockId
          -> condition
          -> Handler stock
        getStockBy pool stockId condition =
          error "`getStockBy` was called. Not implemented yet."
        updateStock ::
             ( StockStorage pool
             , StockPresentable stock
             , StockIdPresentable stockId
             )
          => pool
          -> stockId
          -> stock
          -> Handler String
        updateStock pool stockId stock =
          return "Cannot update Stock, please implement me!"
        deleteStock ::
             (StockStorage pool, StockIdPresentable stockId)
          => pool
          -> stockId
          -> Handler String
        deleteStock pool stockId =
          return "Cannot detele Stock, please implement me!"
