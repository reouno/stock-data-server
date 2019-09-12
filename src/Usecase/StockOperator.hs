{-# LANGUAGE DeriveGeneric #-}

{-
このファイルは多分もう使わない。
Condition --> Usecase.StockCondition
StockStorable --> Entity.Intenal.StockStore （名前は変わっている）
で、あと、ここにあったStockStorableはインタラクターと役割がかぶっていたので、
Usecase.StockStoreAdapterを作って、ここに、StockStoreの実装？でもそうするなら、
インテーフェイスもユースケース側にあった方が良いので、StockStoreはやはりユースケースなのか。
この辺りもう一度考える。
- エンティティとして提供するメソッドのインターフェイスと実装
- ユースケースとして提供するインターフェイスと実装とインタラクター
- コントローラーとして提供するドメインインテーフェイスとプラグインインターフェイスのアダプター


-}
module Usecase.StockOperator
  ( Condition(..)
  , StockStorable(..)
  , PriceType(..)
  , Stock(..)
  , StockId
  ) where

import           Data.Aeson             ( FromJSON, ToJSON )
import           Data.Time              ( Day )
import           Entity.Stock           ( PriceType (..), Stock (..), StockId )
import           GHC.Generics
import           Usecase.StockCondition ( Condition (..) )

class StockStorable storage where
  get :: storage -> StockId -> Stock -- get all data of the stock ID
  getBy :: storage -> Condition -> StockId -> Stock -- get data in specific condition
  add :: storage -> Stock -> IO (Either String StockId) -- add new stock data
  update :: storage -> Stock -> StockId -> IO (Either String StockId) -- update stock data
  delete :: storage -> StockId -> IO (Either String StockId) -- delete stock data
