{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Lib where

import NeatInterpolation (text)

import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.UTF8 as UTF8
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.URLEncoded as UrlE
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Text.Email.Validate as Email
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.HttpAuth (basicAuth)

import Router (Endpoint, dispatch, getEp, postEp, childEp, childEps)
import Middleware (forceTls, prettifyError)

-- Redirect sub-application

githubRedir :: BS.ByteString -> Wai.Application
githubRedir user _ sendResponse =
    sendResponse $ Wai.responseBuilder
      HTTP.status307
      [(HTTP.hLocation, "https://github.com/" <> user)]
      mempty


-- Catch-all sub-application

defaultApp :: Wai.Application
defaultApp req sendResponse =
    sendResponse $ Wai.responseLBS
      HTTP.status200
      [(HTTP.hContentType, "text/plain")]
      (UTF8.fromString . show $ Wai.requestHeaders req)


textResponse' :: HTTP.Status -> LBS.ByteString -> Wai.Application
textResponse' status body _ sendResponse = sendResponse $
    Wai.responseLBS status [(HTTP.hContentType, "text/plain")] body

textResponse :: LBS.ByteString -> Wai.Application
textResponse = textResponse' HTTP.status200

htmlResponse :: Text -> Wai.Application
htmlResponse html _ sendResponse = sendResponse $ Wai.responseLBS
    HTTP.status200
    [(HTTP.hContentType, "text/html; charset=utf-8")]
    (LBS.fromStrict $ encodeUtf8 html)

isPaul :: BS.ByteString -> BS.ByteString -> IO Bool
isPaul "paul" "paul" = pure True
isPaul _ _ = pure False

authMiddleware :: Wai.Middleware
authMiddleware = basicAuth isPaul "Concert API"

interestedSubmissionGet :: Wai.Application
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


-- | This is posted by the web form
interestedCollectionPost :: Wai.Application
interestedCollectionPost req sendResponse = do
    body <- Wai.strictRequestBody req
    let mUrlE = either (const Nothing) Just $
          UrlE.importString $ LUTF8.toString body
    -- FIXME: validation should occur somewhere in between request handling and
    -- storage, but we don't really have a nice place to put it right now.
    -- FIXME: we also want to do client-side JS validation if we can.
    let mEmail =
          mUrlE >>= UrlE.lookup ("email" :: String)
          >>= Email.canonicalizeEmail . UTF8.fromString
    case mEmail of
      (Just canonical) -> sendResponse $ Wai.responseLBS
            HTTP.status200
            [(HTTP.hContentType, "text/plain")]
            (LBS.fromStrict canonical)
      Nothing -> sendResponse $ Wai.responseLBS
        HTTP.status400
        [(HTTP.hContentType, "text/plain")]
        "Invalid email address"


-- | This is used by us to get all the email addresses out
interestedCollectionGet :: Wai.Application
interestedCollectionGet = textResponse "interested collection get"


-- | This sets the users email to verified when they visit
interestedResource :: Text -> Wai.Application
interestedResource name = textResponse $
  "interested resource get: " <> (LBS.fromStrict $ encodeUtf8 name)


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
app = dispatch root

someFunc :: Int -> IO ()
someFunc port = run port $ forceTls $ prettifyError app
