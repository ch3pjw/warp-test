{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Clay (Css, render)
import Control.Applicative ((<|>))
import Control.Monad (join, when)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Data.List (partition, sortBy)
import Data.Monoid
import Data.Pool (Pool)
import Data.String (IsString, fromString)
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
import qualified Network.Wai.Trans as Wai
import Text.BlazeT.Html5 (HtmlT, (!))
import qualified Text.BlazeT.Html5 as H
import Text.BlazeT.Html5.Attributes (href)
import Text.BlazeT.Renderer.Utf8 (renderHtml)
import qualified Text.Email.Validate as Email

import Css
import Registration (
    EmailStore, sPoll, Actor, aSubmitEmailAddress, aVerify, aUnsubscribe,
    usEmailAddress)
import ReadView (
    emailRegistrationEmailAddress,
    EntityField(EmailRegistrationId, EmailRegistrationVerified))
import qualified Templates
import Router
import Middleware (replaceHeaders)
import Templates (s, StaticResources)


-- Redirect sub-application

redir :: (Monad m) => BS.ByteString -> Wai.ApplicationT m
redir url _ sendResponse =
    sendResponse $ Wai.responseBuilder
      HTTP.status307 [(HTTP.hLocation, url)] mempty

githubRedir :: (Monad m) => BS.ByteString -> Wai.ApplicationT m
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

interestedSubmissionGet :: (MonadReader StaticResources m) => Wai.ApplicationT m
interestedSubmissionGet = htmlResponse $ Templates.emailSubmission False


-- | This is posted by the web form
interestedCollectionPost
  :: (MonadReader StaticResources m, MonadIO m)
  => Actor -> EmailStore -> Wai.ApplicationT m
interestedCollectionPost actor store req sendResponse = do
    body <- liftIO $ Wai.strictRequestBody req
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
            liftIO $ aSubmitEmailAddress actor store $ decodeUtf8 canonical
            submissionResponse (decodeUtf8 canonical) req sendResponse
      Nothing ->
        htmlResponse' HTTP.status400 (Templates.emailSubmission True)
        req sendResponse


-- | This is used by us to get all the email addresses out
interestedCollectionGet
  :: (MonadIO m) => Pool DB.SqlBackend -> Wai.ApplicationT m
interestedCollectionGet pool req sendResponse = do
  entities <- liftIO $ DB.runSqlPool
      (DB.selectList
         [EmailRegistrationVerified ==. True]
         [DB.Asc EmailRegistrationId])
      pool
  jsonResponse
    (emailRegistrationEmailAddress . DB.entityVal <$> entities)
    req sendResponse


submissionResponse
  :: (MonadReader StaticResources m) => Text -> Wai.ApplicationT m
submissionResponse email = htmlResponse $
  Templates.emailSubmissionConfirmation email


unsubscriptionResponse :: (MonadReader StaticResources m) => Wai.ApplicationT m
unsubscriptionResponse = htmlResponse Templates.emailUnsubscriptionConfirmation


verificationResponse :: (MonadReader StaticResources m) => Wai.ApplicationT m
verificationResponse = htmlResponse Templates.emailVerificationConfirmation


-- FIXME: this might want to be from the environment
helpEmailAddress :: (IsString a) => a
helpEmailAddress = "hello@concertdaw.co.uk"

-- | This sets the users email to verified when they visit
interestedResource
  :: (MonadReader StaticResources m, MonadIO m) => Actor -> EmailStore -> Text
  -> Wai.ApplicationT m
interestedResource actor store name req sendResponse =
    go (getVerb req) (UUID.fromText name)
  where
    go (Just "verify") (Just uuid) = do
        present <- hasEmail uuid
        if present
        then do
          liftIO $ aVerify actor store uuid
          verificationResponse req sendResponse
        else htmlResponse' HTTP.status404 verErrHtml req sendResponse
    go (Just "verify") Nothing =
        htmlResponse' HTTP.status404 verErrHtml req sendResponse
    go (Just "unsubscribe") (Just uuid) = do
        present <- hasEmail uuid
        when present $ liftIO $ aUnsubscribe actor store uuid
        unsubscriptionResponse req sendResponse
    go (Just "unsubscribe") Nothing = unsubscriptionResponse req sendResponse
    go _ Nothing = htmlResponse' HTTP.status404 genericErrHtml req sendResponse
    go _ _ = htmlResponse' HTTP.status400 genericErrHtml req sendResponse
    getVerb = join . lookup "action" . HTTP.queryToQueryText . Wai.queryString
    -- FIXME: this poll is a bit of a hack; when we have a read view, we should
    -- really be querying that.
    hasEmail uuid = liftIO (sPoll store uuid) >>=
        return . not . Text.null . usEmailAddress
    verErrHtml =
      Templates.page ("Verification Failure") (Just notificationCss)
      Nothing $ do
        H.h1 $ "Verification failed"
        H.p $ do
          s "We didn't recognise your verification link. "
          s "Links expire after 24 hours, but you can always "
          H.a ! href "/interested" $ "resubmit"
          " your email address."
    genericErrHtml =
      Templates.page ("Unrecognised Link") (Just notificationCss) Nothing $ do
        H.h1 $ "Unrecognised link"
        H.p $ do
          s "We didn't recognise the subscription link you visted. You can "
          s "always try "
          H.a ! href "/interested" $ "resubmitting"
          " your email address."
        H.p $ do
          s "If you need some help, get in touch at "
          H.a ! href (mailto helpEmailAddress "Signup%20help") $
            H.text helpEmailAddress
          "."
    mailto addr subj = "mailto:" <> addr <> "?Subject=" <> subj


screenCss :: (Monad m) => Wai.ApplicationT m
screenCss = cssResponse . flattenResponsive 600 $ mainLayout <> mainStyling


templatedErrorTransform
  :: (Monad m) => (HTTP.Status -> [BS.ByteString] -> HtmlT m ()) -> Wai.Response
  -> m Wai.Response
templatedErrorTransform t response =
    Wai.responseLBS status newHdrs <$> H.execWith renderHtml html
  where
    status = Wai.responseStatus response
    (errMsgHdrs, regularHdrs) =
        partition isErrMsg $ Wai.responseHeaders response
    isErrMsg = (== "X-Error-Message") . fst
    newHdrs = replaceHeaders
        (HTTP.hContentType, "text/html; charset=utf-8")
        regularHdrs
    html = t status $ fmap snd errMsgHdrs


generatePreviews
  :: (Monad m) => [(Text, Wai.ApplicationT m)] -> (Text, Endpoint m)
generatePreviews pairs = ("previews", getEp menu <|> childEps children)
  where
    sorted = sortBy (\a1 a2 -> (fst a1) `compare` (fst a2)) pairs
    menu = htmlResponse $ H.docTypeHtml $ do
      H.head $ do
        H.title $ "Preview Pages"
      H.body $ do
        H.ul $ mconcat links
    sanitise = Text.toLower . Text.replace " " "_"
    link t h = H.li $ H.a ! href (fromString . Text.unpack $ "previews/" <> h) $
        H.text t
    links = zipWith link (fst <$> sorted) (fst <$> children)
    children = bimap sanitise getEp <$> sorted
