{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Lib where

import NeatInterpolation (text)

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Header (RequestHeaders, hContentType, hLocation)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.HttpAuth (basicAuth)

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


-- Error prettifying middleware:
replaceHeaders ::
  (HTTP.HeaderName, BS.ByteString) -> [HTTP.Header] -> [HTTP.Header]
replaceHeaders h@(hName, hValue) = (h:) . filter (\(n, v) -> n /= hName)

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
        (replaceHeaders (hContentType, "text/plain") $
         Wai.responseHeaders response)
        (LBS.fromStrict $ HTTP.statusMessage status)


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


-- Routing faff:

data Endpoint' a = Endpoint'
  { epGet :: Maybe a
  , epHead :: Maybe a
  , epPost :: Maybe a
  , epPut :: Maybe a
  , epDelete :: Maybe a
  , epOptions :: Maybe a
  , epPatch :: Maybe a
  , epGetChild :: Maybe (Text -> Endpoint' a)
  }


instance Functor Endpoint' where
    fmap f ep = ep
      { epGet = f <$> epGet ep
      , epHead = f <$> epHead ep
      , epPost = f <$> epPost ep
      , epPut = f <$> epPut ep
      , epDelete = f <$> epDelete ep
      , epOptions = f <$> epOptions ep
      , epPatch = f <$> epPatch ep
      , epGetChild = (fmap . fmap . fmap) f (epGetChild ep)}


instance Applicative Endpoint' where
    pure a = Endpoint'
      { epGet = Just a
      , epHead = Just a
      , epPost = Just a
      , epPut = Just a
      , epDelete = Just a
      , epOptions = Just a
      , epPatch = Just a
      , epGetChild = Nothing}
    ep1 <*> ep2 = Endpoint'
      { epGet = epGet ep1 <*> epGet ep2
      , epHead = epHead ep1 <*> epHead ep2
      , epPost = epPost ep1 <*> epPost ep2
      , epPut = epPut ep1 <*> epPut ep2
      , epDelete = epDelete ep1 <*> epDelete ep2
      , epOptions = epOptions ep1 <*> epOptions ep2
      , epPatch = epPatch ep1 <*> epPatch ep2
      , epGetChild = Nothing}


instance Alternative Endpoint' where
    empty = Endpoint'
      { epGet = Nothing
      , epHead = Nothing
      , epPost = Nothing
      , epPut = Nothing
      , epDelete = Nothing
      , epOptions = Nothing
      , epPatch = Nothing
      , epGetChild = Nothing}
    ep1 <|> ep2 = Endpoint'
      { epGet = epGet ep1 <|> epGet ep2
      , epHead = epHead ep1 <|> epHead ep2
      , epPost = epPost ep1 <|> epPost ep2
      , epPut = epPut ep1 <|> epPut ep2
      , epDelete = epDelete ep1 <|> epDelete ep2
      , epOptions = epOptions ep1 <|> epOptions ep2
      , epPatch = epPatch ep1 <|> epPatch ep2
      , epGetChild = epGetChild ep1 <|> epGetChild ep2}


type Endpoint = Endpoint' Wai.Application


emptyResponse :: HTTP.Status -> Wai.Response
emptyResponse s = Wai.responseBuilder s [] mempty

methodNotAllowed :: Wai.Application
methodNotAllowed req sendResponse = sendResponse $ emptyResponse HTTP.status405

notFound :: Wai.Application
notFound req sendResponse = sendResponse $ emptyResponse HTTP.status404


getEp :: Wai.Application -> Endpoint
getEp a = empty { epGet = Just a }

headEp :: Wai.Application -> Endpoint
headEp a = empty { epHead = Just a }

postEp :: Wai.Application -> Endpoint
postEp a = empty { epPost = Just a }

putEp :: Wai.Application -> Endpoint
putEp a = empty { epPut = Just a }

deleteEp :: Wai.Application -> Endpoint
deleteEp a = empty { epDelete = Just a }

optionsEp :: Wai.Application -> Endpoint
optionsEp a = empty { epOptions = Just a }

patchEp :: Wai.Application -> Endpoint
patchEp a = empty { epPatch = Just a }

childEp :: (Text -> Endpoint) -> Endpoint
childEp f = empty { epGetChild = Just f }

childEps :: [(Text, Endpoint)] -> Endpoint
childEps eps = empty
  { epGetChild = Just $ \t -> maybe notFoundEp id $ lookup t eps }


getEpApp :: HTTP.Method -> Endpoint -> Maybe Wai.Application
getEpApp method ep
  | method == HTTP.methodGet = epGet ep
  | method == HTTP.methodPost = epPost ep
  | method == HTTP.methodDelete = epDelete ep
  | method == HTTP.methodPut = epPut ep
  | method == HTTP.methodHead = epHead ep
  | method == HTTP.methodOptions = epOptions ep
  | method == HTTP.methodPatch = epPatch ep
  | otherwise = pure methodNotAllowed


notFoundEp :: Endpoint
notFoundEp = pure notFound


type Path = [Text]

dispatchEndpoint :: Endpoint -> Wai.Application
dispatchEndpoint ep req = handler (Wai.pathInfo req) ep req
  where
    handler :: Path -> Endpoint -> Wai.Application
    handler [] ep =
      maybe methodNotAllowed id (getEpApp (Wai.requestMethod req) ep)
    handler (name:names) ep =
      maybe notFound (\f -> (handler names $ f name)) (epGetChild ep)


textResponse' :: HTTP.Status -> LBS.ByteString -> Wai.Application
textResponse' status text req sendResponse = sendResponse $
    Wai.responseLBS status [(hContentType, "text/plain")] text

textResponse = textResponse' HTTP.status200

htmlResponse :: Text -> Wai.Application
htmlResponse html req sendResponse = sendResponse $ Wai.responseLBS
    HTTP.status200
    [(hContentType, "text/html; charset=utf-8")]
    (LBS.fromStrict $ encodeUtf8 html)

isPaul :: BS.ByteString -> BS.ByteString -> IO Bool
isPaul "paul" "paul" = pure True
isPaul _ _ = pure False

authMiddleware :: Wai.Middleware
authMiddleware = basicAuth isPaul "Concert API"

interestedSubmissionGet = htmlResponse [text|
  <html>
    <head>
      <title>Hello world!</title>
    </head>
    <body>
      <form method="post">
        Email address:<br/>
        <input type="text" name="email" /><br/>
        <input type="submit" value="Go!"/>
      </form>
    </body>
  </html>
|]
interestedCollectionGet = textResponse "interested collection get"
interestedCollectionPost = textResponse "interested collection post"
interestedResource :: Text -> Wai.Application
interestedResource name =
   textResponse $ "interested resource get: "


root :: Endpoint
root =
  getEp defaultApp <|>
  childEps
    [ ("david", getEp $ githubRedir "foolswood")
    , ("paul", getEp $ githubRedir "ch3pjw")
    , ("api", authMiddleware <$> childEps
        [("interested",
          getEp interestedCollectionGet
         )]
      )
    , ("interested"
      , getEp interestedSubmissionGet <|>
        postEp interestedCollectionPost <|>
        childEp (getEp . interestedResource)
      )
    ]

-- main application

app :: Wai.Application
app = dispatchEndpoint root

someFunc :: Int -> IO ()
someFunc port = run port $ forceTls $ prettifyError app
