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

textResponse' :: HTTP.Status -> LBS.ByteString -> Wai.Application
textResponse' = respond CTPlainText

textResponse :: LBS.ByteString -> Wai.Application
textResponse = textResponse' HTTP.status200

htmlResponse' :: (Monad m) => HTTP.Status -> HtmlT m () -> Wai.ApplicationT m
htmlResponse' status html req sendResponse = do
    bs <- H.execWith renderHtml html
    respond CTHtml status bs req sendResponse

htmlResponse :: (Monad m) => HtmlT m () -> Wai.ApplicationT m
htmlResponse = htmlResponse' HTTP.status200

cssResponse :: (Monad m) => Css -> Wai.ApplicationT m
cssResponse css = respond CTCss HTTP.status200 (encodeUtf8 $ render css)

jsonResponse :: (Monad m, JSON.ToJSON a) => a -> Wai.ApplicationT m
jsonResponse x = respond CTJson HTTP.status200 (JSON.encode x)

errorResponse
  :: HTTP.Status -> BS.ByteString -> [BS.ByteString] -> Wai.Application
errorResponse status title msgs _ sendResponse =
    sendResponse $ Wai.responseLBS status headers ""
  where
    headers = fmap ((,) "X-Error-Message") $ title : msgs
