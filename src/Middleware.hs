{-# LANGUAGE OverloadedStrings #-}

module Middleware
  (forceTls, prettifyError, prettifyError', replaceHeaders)
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Monoid
import qualified Network.Wai as Wai
import qualified Network.Wai.Trans as Wai
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


hasHeader :: HTTP.HeaderName -> HTTP.ResponseHeaders -> Bool
hasHeader name = any $ (== name) . fst

replaceHeaders ::
  (HTTP.HeaderName, BS.ByteString) -> [HTTP.Header] -> [HTTP.Header]
replaceHeaders h@(hName, _) = (h:) . filter (\(n, _) -> n /= hName)


-- | Error prettifying middleware
prettifyError :: (Monad m) => Wai.MiddlewareT m
prettifyError = prettifyError' simpleErrorTransform


-- | Similar to Wai.modifyResponse, but works in the transformer stack
modifyResponse
  :: (Monad m) => (Wai.Response -> m Wai.Response) -> Wai.MiddlewareT m
modifyResponse f app req sendResponse = app req $ \r -> f r >>= sendResponse


prettifyError'
  :: (Monad m) => (Wai.Response -> m Wai.Response) -> Wai.MiddlewareT m
prettifyError' errorTransform = modifyResponse f
  where
    f response =
      if shouldTransform response
      then errorTransform response
      else return response
    statusIsError =
      liftM2 (||) HTTP.statusIsClientError HTTP.statusIsServerError
    shouldTransform response =
      statusIsError (Wai.responseStatus response)
      && not (hasHeader HTTP.hContentType (Wai.responseHeaders response))


simpleErrorTransform :: (Monad m) => Wai.Response -> m Wai.Response
simpleErrorTransform response =
  -- FIXME: this should probably do something smarter. It could combine
  -- the response body with a template?
  let status = Wai.responseStatus response in
  return $ Wai.responseLBS
    status
    (replaceHeaders (HTTP.hContentType, "text/plain") $
     Wai.responseHeaders response)
    (LBS.fromStrict $ HTTP.statusMessage status)
