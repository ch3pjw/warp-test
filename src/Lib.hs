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


data Endpoint' a = Endpoint'
  { epGet :: Maybe a
  , epHead :: Maybe a
  , epPost :: Maybe a
  , epPut :: Maybe a
  , epDelete :: Maybe a
  , epOptions :: Maybe a
  , epPatch :: Maybe a
  , epGetChild :: Maybe (Text -> Endpoint)
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
      , epGetChild = Nothing}

type Endpoint = Endpoint' Wai.Application

methodNotAllowed :: Wai.Application
methodNotAllowed = textResponse' HTTP.status405 "not allowed"

notFound :: Wai.Application
notFound = textResponse' HTTP.status404 "not found"

_endpoint :: Wai.Application -> Endpoint
_endpoint a = Endpoint'
  { epGet = Just a
  , epHead = Just a
  , epPost = Just a
  , epPut = Just a
  , epDelete = Just a
  , epOptions = Just a
  , epPatch = Just a
  , epGetChild = Nothing}

endpoint :: Endpoint
endpoint = _endpoint methodNotAllowed



getEp :: Wai.Application -> Endpoint
getEp a = endpoint { epGet = Just a }

headEp :: Wai.Application -> Endpoint
headEp a = endpoint { epHead = Just a }

postEp :: Wai.Application -> Endpoint
postEp a = endpoint { epPost = Just a }

putEp :: Wai.Application -> Endpoint
putEp a = endpoint { epPut = Just a }

deleteEp :: Wai.Application -> Endpoint
deleteEp a = endpoint { epDelete = Just a }

optionsEp :: Wai.Application -> Endpoint
optionsEp a = endpoint { epOptions = Just a }

patchEp :: Wai.Application -> Endpoint
patchEp a = endpoint { epPatch = Just a }


type Path = [Text]

getEpApp :: HTTP.Method -> Endpoint -> Wai.Application
getEpApp method ep
  | method == HTTP.methodGet = f $ epGet ep
  | method == HTTP.methodPost = f $ epPost ep
  | method == HTTP.methodDelete = f $ epDelete ep
  | method == HTTP.methodPut = f $ epPut ep
  | method == HTTP.methodHead = f $ epHead ep
  | method == HTTP.methodOptions = f $ epOptions ep
  | method == HTTP.methodPatch = f $ epPatch ep
  | otherwise = methodNotAllowed
  where
    f = maybe methodNotAllowed id


notFoundEp :: Endpoint
notFoundEp = _endpoint notFound

endpoints :: [(Text, Endpoint)] -> Text -> Endpoint
endpoints eps t = maybe notFoundEp id $ lookup t eps


dispatchEndpoint :: Endpoint -> Wai.Application
dispatchEndpoint ep req = handler (Wai.pathInfo req) ep req
  where
    handler :: Path -> Endpoint -> Wai.Application
    handler [] ep = getEpApp (Wai.requestMethod req) ep
    handler (name:names) ep =
      maybe notFound (\f -> (handler names $ f name)) (epGetChild ep)


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
    { epGet = Just defaultApp
    , epGetChild = Just $ endpoints
        [ ("david", endpoint {epGet = Just $ githubRedir "foolswood"})
        , ("paul", endpoint {epGet = Just $ githubRedir "ch3pjw"})
        , ("interested", endpoint
            { epGet = Just $ authMiddleware $ interestedCollectionGet
            , epPost = Just interestedCollectionPost
            , epGetChild = Just $
                \name -> endpoint { epGet = Just $ interestedResource name }
            }
          )
        ]
    }

-- main application

app :: Wai.Application
app = dispatchEndpoint root

someFunc :: Int -> IO ()
someFunc port = run port $ forceTls app
