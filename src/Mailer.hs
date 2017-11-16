{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mailer
  ( SmtpSettings(..)
  , generateEmail
  , mailer
  ) where

import Control.Exception (catch, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.DateTime (getCurrentTime)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Format (format)
import Data.UUID (UUID)
import System.Envy (FromEnv, fromEnv, env, envMaybe)

import qualified Network.HaskellNet.SMTP.SSL as SMTP
import qualified Network.Mail.Mime as Mime

import Events
  ( unUuidFor
  , RegistrationEmailType(..)
  , Event
  , EmailEvent(EmailSentEmailEvent)
  , emailEventToEvent
  , timeStamp
  , logEvents
  )
import Store (Store)
import Registration (
  EmailState(..), TimeStamped,
  initialEmailProjection, liftProjection, liftEventT,
  condenseConsecutive, reactivelyRunEventTWithState,
  unsafeEventToEmailEvent)
import Types (Password(..), EnvToggle(..))


data SmtpSettings = SmtpSettings
  { ssServer :: String
  , ssUsername :: String
  , ssPassword :: Password String
  , ssSettings :: SMTP.Settings
  } deriving (Show)

-- FIXME: we could extend this not to assume a port or using TLS (c.f STARTTLS)
instance FromEnv SmtpSettings where
    fromEnv = SmtpSettings
        <$> env "SMTP_SERVER"
        <*> env "SMTP_USERNAME"
        <*> env "SMTP_PASSWORD"
        <*> (envMaybe "SMTP_LOGGING" >>= return . maybe
             (mkSmtpSettings False) (mkSmtpSettings . unEnvToggle))
      where
        mkSmtpSettings logging = SMTP.defaultSettingsSMTPSSL
          { SMTP.sslLogToConsole = logging }

newtype SenderAddress = SenderAddress
  { unSenderAddress :: Mime.Address
  } deriving (Eq, Show)

senderAddress :: Maybe Text -> Text -> SenderAddress
senderAddress name addr = SenderAddress $ Mime.Address name addr

instance FromEnv SenderAddress where
    fromEnv = senderAddress
        <$> envMaybe "MAILER_SENDER_NAME"
        <*> env "MAILER_SENDER_ADDRESS"

type LinkFormatter = UUID -> Text


sendEmails :: SmtpSettings -> [Mime.Mail] -> IO ()
sendEmails _ [] = return ()
sendEmails settings emails =
    SMTP.doSMTPSSLWithSettings
      (ssServer settings)
      (ssSettings settings) $ \conn -> do
    authSuccess <- SMTP.authenticate
      SMTP.LOGIN (ssUsername settings) (unPassword $ ssPassword settings) conn
    if authSuccess
      then mapM_ (flip SMTP.sendMimeMail2 conn) emails
      -- FIXME: fail?
      else putStrLn "SMTP: authentication error."


verificationEmail :: Mime.Address -> Mime.Address -> Text -> Text -> Mime.Mail
verificationEmail from to verificationLink unsubscribeLink =
    Mime.simpleMail' to from subject body
  where
    subject = "Please verify your email address"
    -- FIXME: format isn't type-checked, so we need to write tests for these
    -- functions to make sure we haven't mis-matched our arguments
    body = format
      (  "Hi,\n\n"
      <> "Thanks for registering your interest in Concert! Please verify your "
      <> "email address using the link below:\n\n{}\n\n"
      <> "Thanks!\nThe Concert Team\n\n"
      <> "Your verification link will remain active for 24 hrs\n"
      <> "If you don't want to hear from us any more, you can use the "
      <> "following link to unsubscribe at any time:\n\n{}\n\n")
      [verificationLink, unsubscribeLink]

confirmationEmail :: Mime.Address -> Mime.Address -> Text -> Mime.Mail
confirmationEmail from to unsubscribeLink =
    Mime.simpleMail' to from subject body
  where
    subject = "Subscription confirmed"
    body = format
      (  "Hi,\n\n"
      <> "You've already registered your interest in Concert. We'll let you "
      <> "know when we've got news about our software or we need volunteers to "
      <> "beta test our latest release.\n\n"
      <> "Thanks!\nThe Concert Team\n\n"
      <> "If you don't want to hear from us any more, you can use the "
      <> "following link to unsubscribe at any time:\n\n{}\n\n")
      [unsubscribeLink]


generateEmail
  :: SenderAddress -> LinkFormatter -> LinkFormatter -> UUID -> EmailState
  -> RegistrationEmailType -> Mime.Mail
generateEmail senderAddr verifyLF unsubLF uuid userState emailType =
    case emailType of
      VerificationEmail -> verificationEmail from to vl ul
      ConfirmationEmail -> confirmationEmail from to ul
  where
    to = Mime.Address Nothing $ esEmailAddress userState
    from = unSenderAddress senderAddr
    vl = verifyLF uuid
    ul = unsubLF uuid


mailer
  :: (UUID -> EmailState -> RegistrationEmailType -> Mime.Mail)
  -> SmtpSettings -> Store (TimeStamped Event) -> IO ()
mailer genEmail settings = reactivelyRunEventTWithState
    (liftProjection unsafeEventToEmailEvent initialEmailProjection)
    getSendingEventT
  where
    getSendingEventT uuid' state =
      timeStamp getCurrentTime $ liftEventT emailEventToEvent $
        (liftIO $ doSending uuid' state) >>= logEvents
    doSending uuid' state =
      let
        pending = condenseConsecutive $ esPendingEmails state
        emails = genEmail (unUuidFor uuid') state <$> pending
      in
        -- FIXME: sendEmails CAN FAIL! E.g. if the server is dead or we send an
        -- email to an invalid address. Because we're just reacting to new
        -- events, nothing will go through and send any pending emails that we
        -- dropped.
        sendEmails settings emails >> return (EmailSentEmailEvent <$> pending)
        `catch`
        (\(_ :: SomeException) -> return [])
