module Main where

import           InterfaceAdapter.StockAPIServer (app)
import           Network.Wai.Handler.Warp  (defaultSettings, runSettings,
                                            setBeforeMainLoop, setPort)
import           System.IO                 (hPutStrLn, stderr)

main :: IO ()
main = do
  let port = 8080
      settings =
        setPort port $
        setBeforeMainLoop
          (hPutStrLn stderr ("Listening on port" ++ show port ++ "..."))
          defaultSettings
  runSettings settings =<< app
