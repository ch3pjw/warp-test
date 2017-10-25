{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Prelude hiding (div)
import Control.Monad
import qualified Clay
import qualified Data.ByteString as BS
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (toStrict)
import qualified Network.HTTP.Types as HTTP
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (customParent, MarkupM)

import Css

id_ :: AttributeValue -> Attribute
id_ = A.id

emailSubmission :: Bool -> Html
emailSubmission emailError =
  page "Register Interest" (Just emailSubmissionCss) $ do
    div ! id_ "description" $ do
      h1 $ do
        "Collaborative Audio Production"
      p $ do
        "We're building tools to help you work on audio projects together "
        "across the internet."
      p $ do
        "Our software is "
        a ! href "https://github.com/concert" $ do
          "open source"
        " wherever possible so that you are free to use it however you "
        "like and you'll always be able to access your data."
      p $ do
        "Sign up to our pre-release mailing list to register interest in "
        "beta testing."
    H.form ! method "post" ! id_ "registration-form" $ do
      emailInput
      input ! type_ "submit" ! value "Sign up for updates"
      aside $ do
        "We'll only contact you about service updates and the chance to "
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

emailSubmissionConfirmation :: Text -> Html
emailSubmissionConfirmation email =
  page "Verification Sent" (Just notificationCss) $ do
    h1 "Please Verify"
    p $ do
      "We sent a verification link to "
      text email
      ". Please check your inbox."

emailVerificationConfirmation :: Html
emailVerificationConfirmation =
  page "Regsitered" (Just notificationCss) $ do
    h1 "Registered"
    p "Thank you for verifying your email address."
    p $ do
      "We'll email you when we've got news about our software or when we're "
      "looking for beta testers."

emailUnsubscriptionConfirmation :: Html
emailUnsubscriptionConfirmation =
  page "Unsubscribed" (Just notificationCss) $ do
    h1 "Thank you!"
    p "Thanks for being interested in Concert."
    p "We've removed your email address from our list. Sorry to see you go."


page :: Text -> Maybe ResponsiveCss -> Html -> Html
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
        maybe
          (return ())
          (H.style . text . toStrict . Clay.render . flattenResponsive 600)
          pageCss

    pageHeader =
        header ! id_ "header-wrapper" $ do
          div ! id_ "header" $ do
            a ! href "/" $ do
              picture $ do
                source
                img
            a ! href "/about" $ "About Us"

    contentWrapper =
        div ! id_ "content-wrapper" $ do
          div ! id_ "content" $ do
            pageContent

    pageFooter =
        footer ! id_ "footer-wrapper" $ do
          div ! id_ "footer" $ do
            ul ! id_ "links" $ do
              li $ a ! href "https://medium.com/@concertdaw" $ "Blog"
              li $ a ! href "https://twitter.com/@concertdaw" $ "Twitter"
              li $ a ! href "https://github.com/concert" $ "Github"
            div ! id_ "copyright" $ do
              "Copyright "
              copy
              " 2017 "
              a ! href "/company" $ do
                intersperseM
                   nbsp ["Concert", "Audio", "Technologies", "Limited"]

errorTemplate :: HTTP.Status -> [BS.ByteString] -> Html
errorTemplate status errMsgs =
  let
    s = decodeUtf8 $ HTTP.statusMessage status
    errMsgs' = fmap decodeUtf8 errMsgs
  in
    case errMsgs' of
      [] -> page s (Just notificationCss) $ h1 (text s)
      (m:ms) -> page m (Just notificationCss) $ do
        h1 $ text m
        mapM_ (p . text) ms


pretty404 :: Html
pretty404 = undefined

picture :: Markup -> Markup
picture = customParent "picture"

intersperseM :: (Monad m) => m a -> [m a] -> m ()
intersperseM _ [] = return ()
intersperseM _ (m:[]) = void m
intersperseM m1 (m2:ms) = m2 >> m1 >> intersperseM m1 ms
