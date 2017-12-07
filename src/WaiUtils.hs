{-# LANGUAGE OverloadedStrings #-}

module WaiUtils where

import Clay (Css, render)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai.Trans as Wai
import Text.BlazeT.Html5 (HtmlT)
import qualified Text.BlazeT.Html5 as H
import Text.BlazeT.Renderer.Utf8 (renderHtml)

data ContentType = CTPlainText | CTJson | CTHtml | CTCss

ctString :: ContentType -> BS.ByteString
ctString CTPlainText = "text/plain; charset=utf-8"
ctString CTJson = "application/json; charset=utf-8"
ctString CTHtml = "text/html; charset=utf-8"
ctString CTCss = "text/css; charset=utf-8"

redir :: (Monad m) => BS.ByteString -> Wai.ApplicationT m
redir url _ sendResponse =
    sendResponse $ Wai.responseBuilder
      HTTP.status307 [(HTTP.hLocation, url)] mempty

respond
  :: (Monad m) => ContentType -> HTTP.Status -> LBS.ByteString
  -> Wai.ApplicationT m
respond contentType status body _ sendResponse =
    sendResponse $ Wai.responseLBS
      status [(HTTP.hContentType, ctString contentType)] body

respondText' :: HTTP.Status -> LBS.ByteString -> Wai.Application
respondText' = respond CTPlainText

respondText :: LBS.ByteString -> Wai.Application
respondText = respondText' HTTP.status200

respondHtml' :: (Monad m) => HTTP.Status -> HtmlT m () -> Wai.ApplicationT m
respondHtml' status html req sendResponse = do
    bs <- H.execWith renderHtml html
    respond CTHtml status bs req sendResponse

respondHtml :: (Monad m) => HtmlT m () -> Wai.ApplicationT m
respondHtml = respondHtml' HTTP.status200

respondCss :: (Monad m) => Css -> Wai.ApplicationT m
respondCss css = respond CTCss HTTP.status200 (encodeUtf8 $ render css)

respondJson :: (Monad m, JSON.ToJSON a) => a -> Wai.ApplicationT m
respondJson x = respond CTJson HTTP.status200 (JSON.encode x)

respondError
  :: HTTP.Status -> BS.ByteString -> [BS.ByteString] -> Wai.Application
respondError status title msgs _ sendResponse =
    sendResponse $ Wai.responseLBS status headers ""
  where
    headers = fmap ((,) "X-Error-Message") $ title : msgs

sendResponseWithHeader
    :: HTTP.HeaderName -> BS.ByteString -> (Wai.Response -> t)
    -> Wai.Response -> t
sendResponseWithHeader headerName headerValue sendResponse response =
    sendResponse $
        Wai.mapResponseHeaders  ((headerName, headerValue) :) response
