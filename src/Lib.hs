{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Lib where

import Clay (Css, render)
import Control.Monad (join)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Data.Monoid
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.URLEncoded as UrlE
import qualified Data.UUID as UUID
import qualified Database.Persist.Postgresql as DB
import Database.Persist.Postgresql ((==.))
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Email.Validate as Email

import Css
import Registration (
    Store, sPoll, Actor, aSubmitEmailAddress, aVerify, aUnsubscribe,
    usEmailAddress)
import ReadView (
    emailRegistrationEmailAddress,
    EntityField(EmailRegistrationId, EmailRegistrationVerified))
import qualified Templates
import Middleware (replaceHeaders)


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

data ContentType = CTPlainText | CTJson | CTHtml | CTCss

ctString :: ContentType -> BS.ByteString
ctString CTPlainText = "text/plain; charset=utf-8"
ctString CTJson = "application/json; charset=utf-8"
ctString CTHtml = "text/html; charset=utf-8"
ctString CTCss = "text/css; charset=utf-8"

respond :: ContentType -> HTTP.Status -> LBS.ByteString -> Wai.Application
respond contentType status body _ sendResponse =
    sendResponse $ Wai.responseLBS
      status [(HTTP.hContentType, ctString contentType)] body

textResponse' :: HTTP.Status -> LBS.ByteString -> Wai.Application
textResponse' = respond CTPlainText

textResponse :: LBS.ByteString -> Wai.Application
textResponse = textResponse' HTTP.status200

htmlResponse' :: HTTP.Status -> Html -> Wai.Application
htmlResponse' status html = respond CTHtml status $ renderHtml html

htmlResponse :: Html -> Wai.Application
htmlResponse = htmlResponse' HTTP.status200

cssResponse :: Css -> Wai.Application
cssResponse css = respond CTCss HTTP.status200 (encodeUtf8 $ render css)

jsonResponse :: (JSON.ToJSON a) => a -> Wai.Application
jsonResponse x = respond CTJson HTTP.status200 (JSON.encode x)

errorResponse
  :: HTTP.Status -> BS.ByteString -> [BS.ByteString] -> Wai.Application
errorResponse status title msgs _ sendResponse =
    sendResponse $ Wai.responseLBS status headers ""
  where
    headers = fmap ((,) "X-Error-Message") $ title : msgs

interestedSubmissionGet :: Wai.Application
interestedSubmissionGet = htmlResponse $ Templates.emailSubmission False


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
      Nothing ->
        htmlResponse' HTTP.status400 (Templates.emailSubmission True)
        req sendResponse


-- | This is used by us to get all the email addresses out
interestedCollectionGet :: Pool DB.SqlBackend -> Wai.Application
interestedCollectionGet pool req sendResponse = do
  entities <- DB.runSqlPool
      (DB.selectList
         [EmailRegistrationVerified ==. True]
         [DB.Asc EmailRegistrationId])
      pool
  jsonResponse
    (emailRegistrationEmailAddress . DB.entityVal <$> entities)
    req sendResponse


submissionResponse :: Text -> Wai.Application
submissionResponse email = htmlResponse $
  Templates.emailSubmissionConfirmation email


unsubscriptionResponse :: Wai.Application
unsubscriptionResponse = htmlResponse Templates.emailUnsubscriptionConfirmation


verificationResponse :: Wai.Application
verificationResponse = htmlResponse Templates.emailVerificationConfirmation


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


screenCss :: Wai.Application
screenCss = cssResponse . flattenResponsive 600 $ mainLayout <> mainStyling


templatedErrorTransform  ::
  (HTTP.Status -> [BS.ByteString] -> Html) -> Wai.Response -> Wai.Response
templatedErrorTransform t response =
    Wai.responseLBS status newHdrs (renderHtml html)
  where
    status = Wai.responseStatus response
    (errMsgHdrs, regularHdrs) =
        partition isErrMsg $ Wai.responseHeaders response
    isErrMsg = (== "X-Error-Message") . fst
    newHdrs = replaceHeaders
        (HTTP.hContentType, "text/html; charset=utf-8")
        regularHdrs
    html = t status $ fmap snd errMsgHdrs
