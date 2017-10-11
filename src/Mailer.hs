{-# LANGUAGE OverloadedStrings #-}

module Mailer where

import Data.Maybe
import Data.DateTime (getCurrentTime)
import Data.Text (Text)
import Data.Text.Format (format)
import Data.UUID (UUID)
import System.Environment (getEnv, lookupEnv)

import qualified Network.HaskellNet.SMTP.SSL as SMTP
import qualified Network.Mail.Mime as Mime

import Registration (
  EmailType(..), UserEvent(Emailed), UserState(..), Store, condenseConsecutive,
  reactivelyRunAction, timeStampedAction)

-- FIXME: we could extend this not to assume a port or using TLS (c.f STARTTLS)
data MailerSettings = MailerSettings
  { msSmtpSslSettings :: SMTP.Settings
  , msServer :: String
  , msUsername :: String
  , msPassword :: String
  , msSenderAddress :: Mime.Address
  , msVerifiationLinkFormatter :: LinkFormatter
  , msUnsubscribeLinkFormatter :: LinkFormatter
  }

instance Show MailerSettings where
  show ms = "poop"

type LinkFormatter = UUID -> Text


settingsFromEnv :: LinkFormatter -> LinkFormatter -> IO MailerSettings
settingsFromEnv formatVLink formatULink =
    buildEnv
      <$> lookupEnv "SMTP_LOGGING"
      <*> getEnv "SMTP_SERVER"
      <*> getEnv "SMTP_USERNAME"
      <*> getEnv "SMTP_PASSWORD"
  where
    buildEnv ml s u p = MailerSettings
      { msSmtpSslSettings = SMTP.defaultSettingsSMTPSSL
        { SMTP.sslLogToConsole = maybe False (not . null) ml }
      , msServer = s
      , msUsername = u
      , msPassword = p
      , msSenderAddress =
          Mime.Address (Just "Concert") "noreply@concertdaw.co.uk"
      , msVerifiationLinkFormatter = formatVLink
      , msUnsubscribeLinkFormatter = formatULink
      }


sendEmails :: MailerSettings -> [Mime.Mail] -> IO ()
sendEmails settings mails =
    SMTP.doSMTPSSLWithSettings
      (msServer settings)
      (msSmtpSslSettings settings) $ \conn -> do
    authSuccess <- SMTP.authenticate
      SMTP.LOGIN (msUsername settings) (msPassword settings) conn
    if authSuccess
      then mapM_ (flip SMTP.sendMimeMail2 conn) mails
      else putStrLn "SMTP: authentication error."


verificationEmail :: Mime.Address -> Mime.Address -> Text -> Text -> Mime.Mail
verificationEmail from to verificationLink unsubscribeLink =
    Mime.simpleMail' to from subject body
  where
    subject = "Please verify your email address"
    -- FIXME: format isn't type-checked, so we need to write tests for these
    -- functions to make sure we haven't mis-matched our arguments
    body = format
      "Some kind of nicely formatted body with {} and {} in the middle"
      [verificationLink, unsubscribeLink]

confirmationEmail :: Mime.Address -> Mime.Address -> Text -> Mime.Mail
confirmationEmail from to unsubscribeLink =
    Mime.simpleMail' to from subject body
  where
    subject = "Subscription confirmed"
    body = format
      "You've already signed up etc... Unsubscribe with {}" [unsubscribeLink]


generateEmail :: MailerSettings -> UUID -> UserState -> EmailType -> Mime.Mail
generateEmail settings uuid userState emailType =
    case emailType of
      VerificationEmail -> verificationEmail from to vl ul
      ConfirmationEmail -> confirmationEmail from to ul
  where
    to = Mime.Address Nothing $ usEmailAddress userState
    from = msSenderAddress settings
    vl = msVerifiationLinkFormatter settings uuid
    ul = msUnsubscribeLinkFormatter settings uuid


mailer :: MailerSettings -> Store -> IO (Maybe UUID) -> IO ()
mailer settings = reactivelyRunAction
    (timeStampedAction getCurrentTime action)
  where
    action uuid userState =
      let
        pending = condenseConsecutive $ usPendingEmails userState
        emails = generateEmail settings uuid userState <$> pending
      in
        sendEmails settings emails >> return (Emailed <$> pending)
