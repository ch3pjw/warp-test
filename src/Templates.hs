{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Templates where

import Prelude hiding (div)
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad.Trans (lift)
import Clay ((?), (-:))
import qualified Clay as C
import qualified Clay.Flexbox as Fb
import qualified Data.Aeson as Json
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
  , davePic :: URI
  , paulPic :: URI
  , companyAddress :: [Text]
  , companyNumber :: Text
  } deriving (Show)

instance FromEnv StaticResources where
    fromEnv = StaticResources
      <$> env "LOGO_URL"
      <*> env "LOGO_AND_TEXT_URL"
      <*> env "FAVICON_URL"
      <*> env "DAVE_PROFILE_PIC_URL"
      <*> env "PAUL_PROFILE_PIC_URL"
      <*> envJson "COMPANY_ADDRESS"
      <*> env "COMPANY_NUMBER"


-- FIXME: this lacks an accompanying signature because I can't get at Envy's
-- Parser
envJson str =
    env str >>=
    either (throwError . (\e -> str ++ ": " ++ e)) return . Json.eitherDecode


emailSubmission :: (MonadReader StaticResources m) => Bool -> HtmlT m ()
emailSubmission emailError =
  page "Register Interest" (Just css) Nothing $ do
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


copy, nbsp, ndash, mdash :: MarkupM ()
copy = preEscapedToHtml ("&copy;" :: Text)
nbsp = preEscapedToHtml ("&nbsp;" :: Text)
ndash = preEscapedToHtml ("&ndash;" :: Text)
mdash = preEscapedToHtml ("&mdash;" :: Text)


emailSubmissionConfirmation
  :: (MonadReader StaticResources m) => Text -> HtmlT m ()
emailSubmissionConfirmation email =
  page "Verification Sent" (Just notificationCss) Nothing $ do
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
  page "Registered" (Just notificationCss) Nothing $ do
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
  page "Unsubscribed" (Just notificationCss) Nothing $ do
    h1 "Bye :-("
    p $ do
      s "We've removed your address from our mailing list. "
      "Thanks for being interested in Concert."
    p $ "Unsubscribed by mistake? "


companyInfo :: (MonadReader StaticResources m) => HtmlT m ()
companyInfo =
    page "Company Information" (Just $ notificationCss <> css) Nothing $ do
      h1 "Company information"
      static <- lift ask
      p $ do
        em "Concert Audio Technologies Limited"
        s " is company number "
        text $ companyNumber static
        s " registered in England and Wales."
      p $ do
        s "It's run by "
        a ! href "/about" $ do
          "actual human beings"
        s ", who are friendly and would love to talk to you."
      p $ do
        s "The best way to reach us is by emailing "
        mailto "hello@concertdaw.co.uk" "Hello Concert"
        s ". However, in physical space you can reach us at the following "
        "postal address:"
      div ! id_ "company-address" $ do
        mapM_ line $ companyAddress static
  where
    line t = text t >> br
    css = globalCss $ do
      "#company-address" ? do
        C.paddingLeft $ C.em 1.5

mailto :: (Monad m) => Text -> Text -> HtmlT m ()
mailto addr subj = a ! href mkHref $ text addr
  where
    mkHref = textValue $ "mailto:" <> addr <> "?Subject=" <> subj

aboutUs :: (MonadReader StaticResources m) => HtmlT m ()
aboutUs =
    page "About us" (Just css) (Just AboutUs) $ do
      h1 "Hello, we're Concert"
      div ! id_ "about-profiles" $ do
        static <- lift ask
        div ! id_ "dave" ! class_ "about-profile" $ do
          a ! href "https://github.com/foolswood"
              ! class_ "profile-pic-cont" $ do
            img ! src (showValue $ davePic static) ! class_ "about-profile-pic"
              ! alt "David Honour"
            h2 $ "David Honour"
          p ! class_ "personal-email" $
            mailto "david@concertdaw.co.uk" "Hi David"
          p $ do
            s "David is a multi-instrumentalist and music producer who happens "
            s "to have trained as a physicist and works as a software "
            s "engineer."
          p $ do
            s "He loves working on low-level realtime systems, "
            s "distributed systems and network protocols "
            mdash
            s " so it's no wonder that he's the mastermind behind our "
            s "networked, distributed audio production system."

        div ! id_ "paul" ! class_ "about-profile" $ do
          a ! href "https://github.com/ch3pjw" ! class_ "profile-pic-cont" $ do
            img ! src (showValue $ paulPic static) ! class_ "about-profile-pic"
              ! alt "Paul Weaver"
            h2 $ "Paul Weaver"
          p ! class_ "personal-email" $
            mailto "paul@concertdaw.co.uk" "Hi Paul"
          p $ do
            s "Paul did a PhD in computational chemistry before realising that "
            s "his real passion was software engineering."
          p $ do
            s "He loves turning big, abstract ideas into usable, rock-solid "
            s "bits of software. He's also a musican and composer, so Concert "
            s "is the perfect setting to combine all his interests."
  where
    css = globalCss (do
        C.h1 ? do
          C.fontSize $ C.em 2.5
        "#about-profiles" ? do
          C.display C.grid
        ".profile-pic-cont" ? do
          C.textDecoration C.none
          C.color C.inherit
        ".about-profile-pic" ? do
          borderRadius' $ C.pct 50
          C.width $ C.pct 75
          C.display C.block
          hMargin C.auto
        ".about-profile" ? C.h2 ? do
          C.textAlign C.center
          C.fontWeight $ C.weight 400
        ".personal-email" ? do
          C.fontSizeCustom C.smaller
          C.textAlign C.center
          C.marginTop $ C.px (-20)
        ".personal-email" ? C.a ? do
          C.color C.gray
          C.textDecoration C.none
      ) <> phoneCss (do
        "#about-profiles" ? do
          "grid-template-columns" -: "auto"
      ) <> largeCss (do
        "#content" ? do
          padding' $ C.px 75
        "#about-profiles" ? do
          "grid-template-columns" -: "50% 50%"
          "grid-column-gap" -: "50px"
      )


blogLink :: (IsString a) => a
blogLink = "https://medium.com/@concertdaw"

twitterLink :: (IsString a) => a
twitterLink = "https://twitter.com/@concertdaw"

githubLink :: (IsString a) => a
githubLink = "https://github.com/concert"

showValue :: (Show a) => a -> AttributeValue
showValue = stringValue . show

data NavBarItem = AboutUs deriving (Enum, Eq, Show)

navBar :: (Monad m) => Maybe NavBarItem -> HtmlT m ()
navBar active = ul ! id_ "nav" $ mapM_ f [(AboutUs, "/about", "About Us")]
  where
    f (x, h, t)
     | (Just x) == active = li $ a ! href h ! class_ "active" $ t
     | otherwise = li $ a ! href h $ t


page
  :: (MonadReader StaticResources m)
  => Text -> Maybe ResponsiveCss -> Maybe NavBarItem
  -> HtmlT m () -> HtmlT m ()
page pageTitle pageCss activeNavBarItem pageContent = docTypeHtml $ do
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
          (H.style . text . toStrict . C.render . flattenResponsive 600)
          pageCss

    pageHeader =
        header ! id_ "header-wrapper" $ do
          div ! id_ "header" $ do
            a ! href "/" $ do
              static <- lift ask
              -- object ! height "33" ! data_ (showValue $ logoUrl static)
              --   ! class_ "small-screen" ! alt "Concert Logo" $ "Concert Logo"
              -- object ! height "33" ! data_ (showValue $ logoAndTextUrl static)
              --   ! class_ "large-screen" ! alt "Concert Logo" $ "Concert Logo"
              object ! height "33" ! data_ (showValue $ logoAndTextUrl static)
                ! alt "Concert Logo" $ "Concert Logo"
            navBar activeNavBarItem

    contentWrapper =
        div ! id_ "content-wrapper" $ do
          div ! id_ "content" $ do
            pageContent

    pageFooter =
        footer ! id_ "footer-wrapper" $ do
          div ! id_ "footer" $ do
            ul ! id_ "nav" $ do
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
      [] -> page sMsg (Just notificationCss) Nothing $ h1 (text sMsg)
      (m:ms) -> page m (Just notificationCss) Nothing $ do
        h1 $ text m
        mapM_ (p . text) ms


pretty404 :: (Monad m) => HtmlT m ()
pretty404 = undefined

intersperseM :: (Monad m) => m a -> [m a] -> m ()
intersperseM _ [] = return ()
intersperseM _ (m:[]) = void m
intersperseM m1 (m2:ms) = m2 >> m1 >> intersperseM m1 ms
