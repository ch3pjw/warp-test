{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Lib where

import NeatInterpolation (text)

import Control.Monad (join)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Data.Monoid
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.Format (format)
import Data.Text.Lazy (toStrict)
import qualified Data.URLEncoded as UrlE
import qualified Data.UUID as UUID
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Text.Email.Validate as Email

import Registration (
  Store, sPoll, Actor, aSubmitEmailAddress, aVerify, aUnsubscribe,
  usEmailAddress)

-- Redirect sub-application

redir :: BS.ByteString -> Wai.Application
redir url _ sendResponse =
    sendResponse $ Wai.responseBuilder
      HTTP.status307 [(HTTP.hLocation, url)] mempty

githubRedir :: BS.ByteString -> Wai.Application
githubRedir user = redir $ "https://github.com/" <> user


-- Catch-all sub-application

defaultApp :: Wai.Application
defaultApp req sendResponse =
    sendResponse $ Wai.responseLBS
      HTTP.status200
      [(HTTP.hContentType, "text/plain")]
      (LUTF8.fromString . show $ Wai.requestHeaders req)


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

jsonResponse :: (JSON.ToJSON a) => a -> Wai.Application
jsonResponse a _ sendResponse = sendResponse $ Wai.responseLBS
    HTTP.status200
    [(HTTP.hContentType, "application/json")]
    (JSON.encode a)

interestedSubmissionGet :: Wai.Application
interestedSubmissionGet = htmlResponse [text|
  <html>
    <head>
      <title>Register Interest - Concert</title>
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
interestedCollectionPost :: Actor -> Store -> Wai.Application
interestedCollectionPost actor store req sendResponse = do
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
      (Just canonical) -> do
            aSubmitEmailAddress actor store $ decodeUtf8 canonical
            submissionResponse (decodeUtf8 canonical) req sendResponse
      Nothing -> sendResponse $ Wai.responseLBS
        HTTP.status400
        [(HTTP.hContentType, "text/plain")]
        "Invalid email address"


-- | This is used by us to get all the email addresses out
interestedCollectionGet :: Wai.Application
interestedCollectionGet = textResponse "interested collection get"


submissionResponse :: Text -> Wai.Application
submissionResponse email = htmlResponse (toStrict $ format f [email])
  where
    f = fromString $ Text.unpack [text|
      <html>
        <head>
          <title>Concert</title>
        </head>
        <body>
          <h1>Concert</h1>
          <p>We sent a verification link to {}. Please check your inbox.</p>
        </body>
      </html>|]


unsubscriptionResponse :: Wai.Application
unsubscriptionResponse = htmlResponse [text|
  <html>
    <head>
      <title>Concert</title>
    </head>
    <body>
      <h1>Concert</h1>
      <p>We've removed your email address from our list. Sorry to see you go.
      </p>
    </body>
  </html>
|]


verificationResponse :: Wai.Application
verificationResponse = htmlResponse [text|
  <html>
    <head>
      <title>Concert</title>
    </head>
    <body>
      <h1>Concert</h1>
      <p>Thanks! We'll email you when we've got news about our software or when
      we need people for beta testing.
      </p>
    </body>
  </html>
|]


-- | This sets the users email to verified when they visit
interestedResource :: Actor -> Store -> Text -> Wai.Application
interestedResource actor store name req sendResponse =
  case UUID.fromText name of
    Nothing -> invalidUuidResp
    (Just uuid) -> do
      present <- hasEmail uuid
      if present
        then act (getVerb req) uuid
        else invalidUuidResp
  where
    getVerb = join . lookup "action" . HTTP.queryToQueryText . Wai.queryString
    act mVerb uuid = case mVerb of
        (Just "verify") -> aVerify actor store uuid
            >> verificationResponse req sendResponse
        (Just "unsubscribe") -> aUnsubscribe actor store uuid
            >> unsubscriptionResponse req sendResponse
        _ -> textResponse' HTTP.status400 "Unrecognised action" req sendResponse
    invalidUuidResp = textResponse' HTTP.status404 "Invalid ID" req sendResponse
    -- FIXME: this poll is a bit of a hack; when we have a read view, we should
    -- really be querying that.
    hasEmail uuid = sPoll store uuid >>=
        return . not . Text.null . usEmailAddress
