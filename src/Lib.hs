{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Control.Applicative ((<|>))
import Control.Monad (join, when)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Data.List (partition, sortBy)
import Data.Monoid
import Data.Pool (Pool)
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
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
import Events (UuidFor(..))
import Registration (
    EmailActor, aPoll, aSubmitEmailAddress, aVerify, aUnsubscribe,
    esEmailAddress, emailRegistrationEmailAddress,
    EntityField(EmailRegistrationId, EmailRegistrationVerified))
import qualified Templates
import Router
import Middleware (replaceHeaders)
import Templates (s, StaticResources)
import WaiUtils (redir, respondHtml, respondHtml', respondJson, respondCss)


-- Redirect sub-application

githubRedir :: (Monad m) => BS.ByteString -> Wai.ApplicationT m
githubRedir user = redir $ "https://github.com/" <> user


-- Catch-all sub-application

defaultApp :: Wai.Application
defaultApp req sendResponse =
    sendResponse $ Wai.responseLBS
      HTTP.status200
      [(HTTP.hContentType, "text/plain")]
      (LUTF8.fromString . show $ Wai.requestHeaders req)


interestedSubmissionGet :: (MonadReader StaticResources m) => Wai.ApplicationT m
interestedSubmissionGet = respondHtml $ Templates.emailSubmission False


-- | This is posted by the web form
interestedCollectionPost
  :: (MonadReader StaticResources m, MonadIO m)
  => EmailActor -> Wai.ApplicationT m
interestedCollectionPost actor req sendResponse = do
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
            liftIO $ aSubmitEmailAddress actor $ decodeUtf8 canonical
            submissionResponse (decodeUtf8 canonical) req sendResponse
      Nothing ->
        respondHtml' HTTP.status400 (Templates.emailSubmission True)
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
  respondJson
    (emailRegistrationEmailAddress . DB.entityVal <$> entities)
    req sendResponse


submissionResponse
  :: (MonadReader StaticResources m) => Text -> Wai.ApplicationT m
submissionResponse email = respondHtml $
  Templates.emailSubmissionConfirmation email


unsubscriptionResponse :: (MonadReader StaticResources m) => Wai.ApplicationT m
unsubscriptionResponse = respondHtml Templates.emailUnsubscriptionConfirmation


verificationResponse :: (MonadReader StaticResources m) => Wai.ApplicationT m
verificationResponse = respondHtml Templates.emailVerificationConfirmation


-- FIXME: this might want to be from the environment
helpEmailAddress :: (IsString a) => a
helpEmailAddress = "hello@concertdaw.co.uk"

-- | This sets the users email to verified when they visit
interestedResource
  :: (MonadReader StaticResources m, MonadIO m) => EmailActor -> Text
  -> Wai.ApplicationT m
interestedResource actor name req sendResponse =
    -- FIXME: UuidFor makes the dangerous assumption that a valid UUID points to
    -- a stream of the correct type...
    go (getVerb req) (UuidFor <$> UUID.fromText name)
  where
    go (Just "verify") (Just uuid) = do
        present <- hasEmail uuid
        if present
        then do
          -- FIXME: this now comes back with a result saying if you've
          -- successfully verified:
          liftIO $ aVerify actor uuid
          verificationResponse req sendResponse
        else respondHtml' HTTP.status404 verErrHtml req sendResponse
    go (Just "verify") Nothing =
        respondHtml' HTTP.status404 verErrHtml req sendResponse
    go (Just "unsubscribe") (Just uuid) = do
        present <- hasEmail uuid
        when present $ liftIO $ aUnsubscribe actor uuid
        unsubscriptionResponse req sendResponse
    go (Just "unsubscribe") Nothing = unsubscriptionResponse req sendResponse
    go _ Nothing = respondHtml' HTTP.status404 genericErrHtml req sendResponse
    go _ _ = respondHtml' HTTP.status400 genericErrHtml req sendResponse
    getVerb = join . lookup "action" . HTTP.queryToQueryText . Wai.queryString
    -- FIXME: this poll is a bit of a hack; when we have a read view, we should
    -- really be querying that.
    hasEmail uuid = liftIO (aPoll actor uuid) >>=
        return . not . Text.null . esEmailAddress
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
screenCss = respondCss . flattenResponsive 600 $ mainLayout <> mainStyling


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
    menu = respondHtml $ H.docTypeHtml $ do
      H.head $ do
        H.title $ "Preview Pages"
      H.body $ do
        H.ul $ mconcat links
    sanitise = Text.toLower . Text.replace " " "_"
    link t h = H.li $ H.a ! href (fromString . Text.unpack $ "previews/" <> h) $
        H.text t
    links = zipWith link (fst <$> sorted) (fst <$> children)
    children = bimap sanitise getEp <$> sorted
