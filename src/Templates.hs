{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Data.Text (Text)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

emailSubmission :: Html
emailSubmission = docTypeHtml $ do
  H.head $ do
    H.title "Register Interest - Concert"
  body $ do
    H.form ! method "post" $ do
      "Email address:"
      br
      input ! type_ "text" ! name "email"
      br
      input ! type_ "submit" ! value "Go!"

emailSubmissionConfirmation :: Text -> Html
emailSubmissionConfirmation email = docTypeHtml $ do
  H.head $ do
    H.title "Concert"
  body $ do
    h1 "Concert"
    p $ do
      "We sent a verification link to "
      text email
      ". Please check your inbox."

emailVerificationConfirmation :: Html
emailVerificationConfirmation = docTypeHtml $ do
  H.head $ do
    H.title "Concert"
  body $ do
    h1 "Concert"
    p $ do
      "Thank you for verifying your email address. We'll email you when we've "
      "got news about our software or when we're looking for beta testers."

emailUnsubscriptionConfirmation :: Html
emailUnsubscriptionConfirmation = docTypeHtml $ do
  H.head $ do
    H.title "Concert"
  body $ do
    h1 "Concert"
    p "We've removed your email address from our list. Sorry to see you go."

pretty404 :: Html
pretty404 = undefined
