module Main where

import           Controller.StockServer   ( app )
import           Network.Wai.Handler.Warp ( run )

main :: IO ()
main = do
  putStrLn "Hello, I'm Stock API server"
  run 8080 app
