{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Header (hContentType)

someFunc :: Int -> IO ()
someFunc port = run port $ \req f ->
  do
    putStrLn . show $ Wai.requestHeaders req
    f $ Wai.responseLBS HTTP.status200 [(hContentType, "text/plain")] "Hello World!"
