{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Templates where

import Prelude hiding (div)
import Control.Monad
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans (lift)
import qualified Clay
import qualified Data.ByteString as BS
import Data.Monoid
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import qualified Network.HTTP.Types as HTTP
import Network.URI (URI)
import Text.BlazeT.Html5 as H
import Text.BlazeT.Html5.Attributes hiding (id)
import qualified Text.BlazeT.Html5.Attributes as A
import Text.BlazeT.Internal (MarkupM)
import System.Envy (FromEnv, fromEnv, env)

import Css
import Types ()

id_ :: AttributeValue -> Attribute
id_ = A.id

-- | A little helper for force consecutive strings in `do` notation inside HtmlT
-- to have type `HtmlT m ()` cf `HtmlT m a`, where Haskell can't then work out
-- what a should be.
s :: (Monad m) => String -> HtmlT m ()
s = H.string

data StaticResources = StaticResources
  { logoUrl :: URI
  , logoAndTextUrl :: URI
  , faviconUrl :: URI
  } deriving (Show)

instance FromEnv StaticResources where
    fromEnv = StaticResources
      <$> env "LOGO_URL"
      <*> env "LOGO_AND_TEXT_URL"
      <*> env "FAVICON_URL"


emailSubmission :: (MonadReader StaticResources m) => Bool -> HtmlT m ()
emailSubmission emailError =
  page "Register Interest" (Just emailSubmissionCss) $ do
    div ! id_ "description" $ do
      h1 $ do
        "Collaborative Audio Production"
      p $ do
        s "We're building tools to help you work on audio projects together "
        "across the internet."
      p $ do
        "We're aiming to let you"
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
        s " wherever possible so that you have a bigger say in how we grow."
      p $ do
        s "Sound interesting? Sign up to our pre-release mailing list for "
        "updates and beta testing opportunities."
    H.form ! method "post" ! id_ "registration-form" $ do
      emailInput
      input ! type_ "submit" ! value "Sign up for updates"
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

nbsp :: MarkupM ()
nbsp = preEscapedToHtml ("&nbsp;" :: Text)

copy :: MarkupM ()
copy = preEscapedToHtml ("&copy;" :: Text)

emailSubmissionConfirmation
  :: (MonadReader StaticResources m) => Text -> HtmlT m ()
emailSubmissionConfirmation email =
  page "Verification Sent" (Just notificationCss) $ do
    h1 "Please verify your address"
    p $ do
      s "We sent a verification link to "
      strong $ text email
      "."
    p $ do
      s "Please check your inbox and visit the link so that we can be sure it's "
      "okay to send you emails."

emailVerificationConfirmation :: (MonadReader StaticResources m) => HtmlT m ()
emailVerificationConfirmation =
  page "Registered" (Just notificationCss) $ do
    h1 "Registered!"
    p "Thanks for verifying your address."
    p $ do
      s "We'll email you when we've got news about our software and when we're "
      "looking for beta testers."
    p $ do
      s "In the meantime, you can "
      a ! href blogLink $ "read our blog"
      s " or "
      a ! href twitterLink $ "follow us on Twitter"
      "."


emailUnsubscriptionConfirmation :: (MonadReader StaticResources m) => HtmlT m ()
emailUnsubscriptionConfirmation =
  page "Unsubscribed" (Just notificationCss) $ do
    h1 "Bye :-("
    p $ do
      s "We've removed your address from our mailing list. "
      "Thanks for being interested in Concert."
    p $ "Unsubscribed by mistake? "


blogLink :: (IsString a) => a
blogLink = "https://medium.com/@concertdaw"

twitterLink :: (IsString a) => a
twitterLink = "https://twitter.com/@concertdaw"

githubLink :: (IsString a) => a
githubLink = "https://github.com/concert"

showValue :: (Show a) => a -> AttributeValue
showValue = stringValue . show

page
  :: (MonadReader StaticResources m) => Text -> Maybe ResponsiveCss
  -> HtmlT m () -> HtmlT m ()
page pageTitle pageCss pageContent = docTypeHtml $ do
    htmlHead
    body $ do
      pageHeader
      contentWrapper
      pageFooter
  where
    titleText = text $
        if Text.null pageTitle then "Concert" else pageTitle <> " - Concert"
    htmlHead = H.head $ do
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        H.title titleText
        link ! rel "stylesheet" ! href "/screen.css"
        -- FIXME: regardless of injecting the location, we still kinda know
        -- magically what type this will be. I don't know if that's an issue...
        static <- lift ask
        link ! rel "icon" ! type_ "image/svg+xml" ! size "any"
          ! href (showValue $ faviconUrl static)
        maybe
          (return ())
          (H.style . text . toStrict . Clay.render . flattenResponsive 600)
          pageCss

    pageHeader =
        header ! id_ "header-wrapper" $ do
          div ! id_ "header" $ do
            a ! href "/" $ do
              static <- lift ask
              object ! height "33" ! data_ (showValue $ logoUrl static)
                ! class_ "small-screen" ! alt "Concert Logo" $ "Concert Logo"
              object ! height "33" ! data_ (showValue $ logoAndTextUrl static)
                ! class_ "large-screen" ! alt "Concert Logo" $ "Concert Logo"
            a ! href "/about" $ "About Us"

    contentWrapper =
        div ! id_ "content-wrapper" $ do
          div ! id_ "content" $ do
            pageContent

    pageFooter =
        footer ! id_ "footer-wrapper" $ do
          div ! id_ "footer" $ do
            ul ! id_ "links" $ do
              li $ a ! href blogLink $ "Blog"
              li $ a ! href twitterLink $ "Twitter"
              li $ a ! href githubLink $ "Github"
            div ! id_ "copyright" $ do
              s "Copyright "
              copy
              s " 2017 "
              a ! href "/company" $ do
                intersperseM
                   nbsp ["Concert", "Audio", "Technologies", "Limited"]

errorTemplate
  :: (MonadReader StaticResources m) => HTTP.Status -> [BS.ByteString]
  -> HtmlT m ()
errorTemplate status errMsgs =
  let
    sMsg = decodeUtf8 $ HTTP.statusMessage status
    errMsgs' = fmap decodeUtf8 errMsgs
  in
    case errMsgs' of
      [] -> page sMsg (Just notificationCss) $ h1 (text sMsg)
      (m:ms) -> page m (Just notificationCss) $ do
        h1 $ text m
        mapM_ (p . text) ms


pretty404 :: (Monad m) => HtmlT m ()
pretty404 = undefined

intersperseM :: (Monad m) => m a -> [m a] -> m ()
intersperseM _ [] = return ()
intersperseM _ (m:[]) = void m
intersperseM m1 (m2:ms) = m2 >> m1 >> intersperseM m1 ms
