{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module InterfaceAdapter.Presenter.StockAPIHandler
  ( StockAPI(..)
  , stockAPI
  , stockServer
  ) where

import           Control.Monad.IO.Class              ( liftIO )
import           Servant
import           Usecase.Interactor.PlainStockServer ( addStockToStore, appendStockPrice2Store,
                                                       deleteStockByStockId, serveStockFromStore )
import           Usecase.Interface.PresentableData   ( StockIdPresentable (..),
                                                       StockPresentable (..) )
import           Usecase.Interface.StockStorage      ( StockStorage )

type StockAPI stock stockId condition
   = Get '[ JSON] String -- for test
      :<|> ReqBody '[ JSON] stock :> Post '[ JSON] stockId -- add stock data
      :<|> Capture "stockId" stockId :> (Get '[ JSON] stock -- get stock data
                                          :<|> ReqBody '[ JSON] condition :> Post '[ JSON] stock -- get stock data by specific condition
                                          :<|> "append" :> ReqBody '[ JSON] stock :> Post '[ JSON] NoContent -- append new data
                                          :<|> ReqBody '[ JSON] stock :> Put '[ JSON] String -- update stock data
                                          :<|> Delete '[ JSON] NoContent -- delete stock data
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
         (StockStorage pool, StockPresentable stock, StockIdPresentable stockId)
      => pool
      -> stock
      -> Handler stockId
    addStock pool stock = liftIO $ addStockToStore pool stock
    stockOperations pool stockId =
      getStock pool stockId :<|> getStockBy pool stockId :<|>
      appendStock pool stockId :<|>
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
        appendStock ::
             ( StockStorage pool
             , StockPresentable stock
             , StockIdPresentable stockId
             )
          => pool
          -> stockId
          -> stock
          -> Handler NoContent
        appendStock pool stockId stock =
          liftIO $ do
            appendStockPrice2Store pool stockId stock
            return NoContent
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
          -> Handler NoContent
        deleteStock pool stockId =
          liftIO $ do
            deleteStockByStockId pool stockId
            return NoContent
