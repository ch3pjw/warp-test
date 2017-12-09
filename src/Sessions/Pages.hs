{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Sessions.Pages where

import Prelude hiding (div)
import Blaze.ByteString.Builder (toByteString)
import Clay ((?), (-:))
import qualified Clay as C
import qualified Clay.Flexbox as Fb
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.URLEncoded as UrlE
import qualified Data.UUID as UUID
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Trans as Wai
import Text.BlazeT.Html5 as H
import Text.BlazeT.Html5.Attributes hiding (id)
import qualified Text.Email.Validate as Email
import Web.ClientSession (Key)
import qualified Web.Cookie as WC

import Css (notificationCss, globalCss, phoneCss, largeCss, padding')
import Events (EmailAddress, UserAgentString(..), SessionEvent)
import Lib (helpEmailAddress)
import Templates (StaticResources, page, s, id_, nbsp, mailto)
import UuidFor (UuidFor(..))
import WaiUtils (respondHtml, respondHtml', sendResponseWithHeader, redir)

import Sessions.Cookies (SessionCookie, sessionSetCookie)


homePageContent :: (MonadReader StaticResources m) => Bool -> HtmlT m ()
homePageContent emailError = page "" (Just css) Nothing $ do
    h1 $ do
      "Introducing Collaborative Audio Production"
    div ! id_ "grid" $ do
      div ! id_ "description" $ do
        p $ do
          s "We're building tools to help you work on audio projects together "
          "across the internet."
        p $ do
          "We're aiming to let you:"
        ul ! class_ "ul-ticks" $ do
          li $ do
            s "Control your session from multiple devices at"
            nbsp
            "once"
          li $ "Invite other people to join your sessions live"
          li $ "Track the changes everyone has made to a project."
        p $ do
          s "Our software is "
          a ! href "https://github.com/concert" $ do
            "open source"
          s " wherever possible so that you have a bigger say in how it grows."
        p $ do
          s "Sound interesting? Sign up for an account and we'll email you "
          "with early announcements and beta testing opportunities."
      H.form ! method "post" ! id_ "registration-form" $ do
        emailInput
        input ! type_ "submit" ! value "Sign in / Sign up"
        aside $ do
          s "We'll only contact you about service updates and the chance to "
          "try out pre-release software."
  where
    emailInput' =
        input ! type_ "email" ! name "email" ! placeholder "name@example.com"
        ! autofocus "true" -- FIXME: is this right?
    emailInput =
        if emailError
        then do
          H.label ! for "email" ! class_ "error" $
            "Please enter a valid email address"
          emailInput' ! class_ "error"
        else do
          H.label ! for "email" $ "Email"
          emailInput'
    css = globalCss (do
        "#grid" ? do
          C.display C.grid

        "#registration-form" ? do
          C.alignSelf C.center

        C.form ? do
          C.display C.flex
          C.flexFlow Fb.column Fb.nowrap
      ) <> phoneCss (do
          "#grid" ? do
            "grid-template-columns" -: "auto"
            "grid-row-gap" -: "20 px"
      ) <> largeCss (do
          "#grid" ? do
            "grid-template-columns" -: "60% 40%"
            "grid-column-gap" -: "50px"
          "#content" ? do
            padding' $ C.px 75
      )


-- | The endpoint that session request form submissions are sent to
signInPost
  :: (MonadIO m, MonadReader StaticResources m)
  => (EmailAddress -> IO ()) -> Wai.ApplicationT m
signInPost requestSession httpReq sendResponse = do
    reqBody <- liftIO $ Wai.strictRequestBody httpReq
    let urlE = either (const mempty) id $ UrlE.importString $
          LUTF8.toString reqBody
    let mEmail =
          UrlE.lookup ("email" :: String) urlE
          >>= Email.canonicalizeEmail . UTF8.fromString
    case mEmail of
      Just canonical ->
        let email = decodeUtf8 canonical in do
          liftIO $ requestSession email
          -- FIXME: this should probably be a redirect to a resource that you
          -- can refresh without re-submitting the request for a new
          -- session. However, to do that properly we'd need to synchronise with
          -- the session process manager having updated the pending sessions.
          respondHtml (successHtml email) httpReq sendResponse
      Nothing -> respondHtml' HTTP.status400 (homePageContent True) httpReq
          sendResponse
  where
    successHtml :: (MonadReader StaticResources m) => Text -> HtmlT m ()
    successHtml email =
        page "Sign-in email sent" (Just notificationCss) Nothing $ do
          h1 "You've got mail"
          p $ do
            s "We sent a sign-in link to "
            strong $ text email
            "."
          p $ do
            s "Please check your inbox and visit the link to sign in."


signInResource
  :: (MonadIO m, MonadReader StaticResources m)
  => Key
  -> (UuidFor SessionEvent -> UserAgentString -> IO (Maybe SessionCookie))
  -> Text -> Wai.ApplicationT m
signInResource cookieEncryptionKey doSignIn uuidText req sendResponse =
    -- FIXME: UuidFor labels the UUID as being for the correct type, regardless
    -- of whether this is true. Don't know if that's going to be an issue...
    go (UuidFor <$> UUID.fromText uuidText)
  where
    go Nothing = respondHtml' HTTP.status404 genericErrHtml req sendResponse
    go (Just sUuid') = do
      mCookie <- liftIO $ doSignIn sUuid' uaString
      mSetCookie <-
          traverse (liftIO . sessionSetCookie cookieEncryptionKey) mCookie
      case mSetCookie of
        -- FIXME: not sure if we want this to be the same generic, and whether
        -- 404 is always the right code, and if that even matters...
        Nothing -> respondHtml' HTTP.status404 genericErrHtml req sendResponse
        Just sCookie ->
          let sCookieBS = toByteString $ WC.renderSetCookie sCookie in
          respondHtml successHtml req $
              sendResponseWithHeader "Set-Cookie" sCookieBS sendResponse
    uaString = UserAgentString $ decodeUtf8 $
        maybe "" id $ Wai.requestHeaderUserAgent req

    genericErrHtml =
      page "Unrecognised link" (Just notificationCss) Nothing $ do
        h1 "Unrecognised link"
        p $ do
          s "We didn't recognise the sign in link you visited. Links expire "
          s "after fifteen minutes, but you "
          s "can always try "
          -- FIXME: implicit path knowledge?
          a ! href "/" $ "signing in"
          " again."
        p $ do
          s "If you need some help, get in touch at "
          mailto helpEmailAddress "Sign%20in%20help"
          "."

    -- FIXME: actual content
    successHtml = page "Have a cookie" (Just notificationCss) Nothing $ do
        h1 "Have a cookie"
        p $ do
          s "It seems that you managed to sign in. Congrats!"


-- FIXME: this must be protected behind something that checks that you've
-- actually got the cookie for the session you are signing out of!
signOutResource
  :: (MonadIO m, MonadReader StaticResources m)
  => (UuidFor SessionEvent -> IO ()) -> Text -> Wai.ApplicationT m
signOutResource doSignOut uuidText req sendResponse =
    go (UuidFor <$> UUID.fromText uuidText)
  where
    go (Just sUuid') = do
      liftIO $ doSignOut sUuid'
      redir "/" req sendResponse
    go Nothing = respondHtml' HTTP.status404 genericErrHtml req sendResponse

    genericErrHtml =
      -- FIXME: make this error correct!
      page "Unrecognised link" (Just notificationCss) Nothing $ do
        h1 "Unrecognised link"
        p $ do
          s "We didn't recognise the sign in (OR OUT?!) link you visited. You "
          s "can always try "
          -- FIXME: implicit path knowledge? Use signInUrl?
          a ! href "/" $ "signing in"
          "again."
        p $ do
          s "If you need some help, get in touch at "
          mailto helpEmailAddress "Sign%20in%20help"
          "."
