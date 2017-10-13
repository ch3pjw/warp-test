{-# LANGUAGE OverloadedStrings #-}

module Middleware
  (forceTls, prettifyError)
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

hasHttpsHeader :: HTTP.RequestHeaders -> Bool
hasHttpsHeader hs = maybe False (== "https") $ lookup "X-Forwarded-Proto" hs

-- | Secure-only redirect middleware
forceTls :: Wai.Middleware
forceTls app req sendResponse =
  if shouldRedirect then sendResponse redirectResponse else app req sendResponse
  where
    shouldRedirect = not . hasHttpsHeader . Wai.requestHeaders $ req
    redirectResponse =
      maybe response400 makeRedirect $ Wai.requestHeaderHost req
    response400 = Wai.responseLBS
      HTTP.status400
      [(HTTP.hContentType, "text/plain")]
      "400: Bad Request: Missing Host header"
    makeRedirect host = Wai.responseBuilder
      (if Wai.requestMethod req == HTTP.methodGet
         then HTTP.status301
         else HTTP.status307)
      [(HTTP.hLocation, location host)]
      mempty
    location host =
      "https://" <> host <> Wai.rawPathInfo req <> Wai.rawQueryString req


replaceHeaders ::
  (HTTP.HeaderName, BS.ByteString) -> [HTTP.Header] -> [HTTP.Header]
replaceHeaders h@(hName, _) = (h:) . filter (\(n, _) -> n /= hName)

-- | Error prettifying middleware
prettifyError :: Wai.Middleware
prettifyError app req sendResponse = app req mySendResponse
  where
    mySendResponse :: Wai.Response -> IO Wai.ResponseReceived
    mySendResponse response =
      let status = Wai.responseStatus response in
      if HTTP.statusIsSuccessful status
      then sendResponse response
      else sendResponse $ Wai.responseLBS
        status
        (replaceHeaders (HTTP.hContentType, "text/plain") $
         Wai.responseHeaders response)
        (LBS.fromStrict $ HTTP.statusMessage status)
