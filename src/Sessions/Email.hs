{-# LANGUAGE OverloadedStrings #-}

module Sessions.Email where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import qualified Network.Mail.Mime as Mime


signInMail :: Mime.Address -> Mime.Address -> Text -> Mime.Mail
signInMail from to signInLink =
    Mime.simpleMail' to from subject body
  where
    subject = "Finish signing in to Concert"
    -- FIXME: format isn't type-checked, so we need to write tests for these
    -- functions to make sure we haven't mis-matched our arguments
    body =
      (  "Click the link below to finish signing in to your account on "
      <> "Concert. The link will expire in 15 minutes and can only be used "
      <> "once.\n\n"
      <> LazyText.fromStrict signInLink
      <> "\n\n"
      <> "Thanks,\nThe Concert Team\n\n"
      <> "If you did not request to sign in, just ignore this email.")
