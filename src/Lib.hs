{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Header (hContentType)

someFunc :: Int -> IO ()
someFunc port = run port $ \req f ->
    f $ Wai.responseLBS HTTP.status200 [(hContentType, "text/plain")] $
        fromString . show $ Wai.requestHeaders req
