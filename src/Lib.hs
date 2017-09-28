{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative ((<|>))
import Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Monoid ((<>))
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Header (RequestHeaders, hContentType, hLocation)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.UrlMap (mapUrls, mount, mountRoot)

-- Secure-only redirect middleware:
hasHttpsHeader :: RequestHeaders -> Bool
hasHttpsHeader hs = maybe False (== "https") $ lookup "X-Forwarded-Proto" hs

forceTls :: Wai.Middleware
forceTls app req sendResponse =
  if shouldRedirect then sendResponse redirectResponse else app req sendResponse
  where
    shouldRedirect = not . hasHttpsHeader . Wai.requestHeaders $ req
    redirectResponse =
      maybe response400 makeRedirect $ Wai.requestHeaderHost req
    response400 = Wai.responseLBS
      HTTP.status400
      [(hContentType, "text/plain")]
      "400: Bad Request: Missing Host header"
    makeRedirect host = Wai.responseBuilder
      (if Wai.requestMethod req == HTTP.methodGet
         then HTTP.status301
         else HTTP.status307)
      [(hLocation, location host)]
      mempty
    location host =
      "https://" <> host <> Wai.rawPathInfo req <> Wai.rawQueryString req


-- Redirect sub-application

githubRedir :: BS.ByteString -> Wai.Application
githubRedir user req sendResponse =
    -- FIXME: only support GET?
    sendResponse $ Wai.responseBuilder
      HTTP.status307
      [(hLocation, "https://github.com/" <> user)]
      mempty


-- Catch-all sub-application

defaultApp :: Wai.Application
defaultApp req sendResponse =
    sendResponse $ Wai.responseLBS
      HTTP.status200
      [(hContentType, "text/plain")]
      (fromString . show $ Wai.requestHeaders req)


-- Main application

app :: Wai.Application
app = mapUrls $
    mount "david" (githubRedir "foolswood")
    <|> mount "paul" (githubRedir "ch3pjw")
    <|> mountRoot defaultApp

someFunc :: Int -> IO ()
someFunc port = run port $ forceTls app
