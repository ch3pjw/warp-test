{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative ((<|>))
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Monoid ((<>))
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Header (RequestHeaders, hContentType, hLocation)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)

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


data Endpoint = Endpoint
  { epGet :: Wai.Application
  , epHead :: Wai.Application
  , epPost :: Wai.Application
  , epPut :: Wai.Application
  , epDelete :: Wai.Application
  , epOptions :: Wai.Application
  , epPatch :: Wai.Application
  , epGetChild :: Maybe (Text -> Endpoint)
  }

methodNotAllowed :: Wai.Application
methodNotAllowed = textResponse' HTTP.status405 "not allowed"

notFound :: Wai.Application
notFound = textResponse' HTTP.status404 "not found"

_endpoint :: Wai.Application -> Endpoint
_endpoint a = Endpoint
  { epGet = a
  , epHead = a
  , epPost = a
  , epPut = a
  , epDelete = a
  , epOptions = a
  , epPatch = a
  , epGetChild = Nothing}

endpoint :: Endpoint
endpoint = _endpoint methodNotAllowed

notFoundEp :: Endpoint
notFoundEp = _endpoint notFound

endpoints :: [(Text, Endpoint)] -> Text -> Endpoint
endpoints eps t = maybe notFoundEp id $ lookup t eps


dispatchEndpoint :: Endpoint -> Wai.Application
dispatchEndpoint ep req = handler (Wai.pathInfo req) ep req
  where
    method = Wai.requestMethod req
    handler [] ep = getEpApp ep
    handler (name:names) ep =
      maybe notFound (\f -> (handler names $ f name)) (epGetChild ep)
    getEpApp ep
      | method == HTTP.methodGet = epGet ep
      | method == HTTP.methodPost = epPost ep
      | method == HTTP.methodDelete = epDelete ep
      | method == HTTP.methodPut = epPut ep
      | method == HTTP.methodHead = epHead ep
      | method == HTTP.methodOptions = epOptions ep
      | method == HTTP.methodPatch = epPatch ep
      | otherwise = methodNotAllowed


textResponse' :: HTTP.Status -> LBS.ByteString -> Wai.Application
textResponse' status text req sendResponse = sendResponse $
    Wai.responseLBS status [(hContentType, "text/plain")] text

textResponse = textResponse' HTTP.status200

authMiddleware = id
interestedCollectionGet = textResponse "interested collection get"
interestedCollectionPost = textResponse "interested collection post"
interestedResource name =
   textResponse $ "interested resource get: "


root :: Endpoint
root = endpoint
    { epGet = defaultApp
    , epGetChild = Just $ endpoints
        [ ("david", endpoint {epGet = githubRedir "foolswood"})
        , ("paul", endpoint {epGet = githubRedir "ch3pjw"})
        , ("interested", endpoint
            { epGet = authMiddleware $ interestedCollectionGet
            , epPost = interestedCollectionPost
            , epGetChild = Just $
                \name -> endpoint { epGet = interestedResource name }
            }
          )
        ]
    }

-- main application

app :: Wai.Application
app = dispatchEndpoint root

someFunc :: Int -> IO ()
someFunc port = run port $ forceTls app
