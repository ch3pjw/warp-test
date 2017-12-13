module Sessions.CommandHandler where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import qualified Network.Mail.Mime as Mime

import Eventful (ExpectedPosition(..))

import Events
  ( EmailAddress, UserAgentString, SessionCommand(..), Event, SessionEvent(..)
  , AccountEvent, TimeStamped, toEvent, decomposeEvent)
import EventT
  ( EventT, logEvents', logEvents_', logWithLatest', getState', mapEvents
  , timeStamp)
import Mailer (SenderAddress(..), SmtpSettings, trySendEmails)
import Store (Store, sRunEventT)
import UuidFor (UuidFor, coerceUuidFor)
import qualified UuidFor as UuidFor

import Sessions.Cookies (SessionCookie, sessionCookie)
import Sessions.Email (signInMail)
import Sessions.Model
  (SessionStage(..), SessionState(..), initialSessionProjection)


type LinkFormatter event = UuidFor event -> Text

-- These functions correspond to all the session commands that can be issued:

requestSessionCommand :: (MonadIO m) => EmailAddress -> EventT SessionEvent m ()
requestSessionCommand e =
    -- Logging events can fail if the stream happens to exist (collision),
    -- so we retry:
    void $ untilRight go
  where
    go = do
      sUuid' <- UuidFor.newRandom
      logEvents' sUuid' NoStream [SessionRequestedSessionEvent e]
    untilRight :: (Monad m) => m (Either a b) -> m b
    untilRight m = m >>= either (const $ untilRight m) return

bindSessionToAccountCommand
    :: (Monad m)
    => UuidFor SessionEvent
    -> UuidFor (TimeStamped AccountEvent)
    -> EventT SessionEvent m ()
bindSessionToAccountCommand sUuid' aUuid' =
    logEvents_' sUuid' AnyPosition
      [SessionBoundToAccountSessionEvent aUuid']

sendSignInEmailCommand
    :: (MonadIO m)
    => SenderAddress -> SmtpSettings -> LinkFormatter SessionEvent
    -> UuidFor SessionEvent -> EmailAddress
    -> EventT SessionEvent m ()
sendSignInEmailCommand senderAddr smtpSettings formatLink sUuid' e = do
    success <- liftIO $ trySendEmails smtpSettings
      [signInMail (unSenderAddress senderAddr) mimeAddr (formatLink sUuid')]
    let event = either
          (SessionSignInEmailSendingFailedSessionEvent . Text.pack)
          (const SessionSignInEmailSentSessionEvent)
          success
    logEvents_' sUuid' AnyPosition [event]
  where
    mimeAddr = Mime.Address Nothing e

expireSessionRequestCommand
    :: (Monad m) => (UuidFor SessionEvent) -> EventT SessionEvent m ()
expireSessionRequestCommand sUuid' = do
    sessionState <- getState' initialSessionProjection sUuid'
    case ssStage sessionState of
      Active -> logExpiry
      Terminated -> logExpiry
      _ -> return ()
  where
    logExpiry = logEvents_' sUuid' AnyPosition
        [SessionSignInWindowExpiredSessionEvent]

signInCommand
    :: (Monad m) => (UuidFor SessionEvent) -> UserAgentString
    -> EventT SessionEvent m (Maybe SessionCookie)
signInCommand sUuid' uaString = logWithLatest' initialSessionProjection sUuid' f
  where
    f (SessionState _ Pending maUuid') =
      case maUuid' of
        Nothing -> nout
        Just aUuid' ->
          ( [SessionSignedInSessionEvent uaString]
          , Just $ sessionCookie (coerceUuidFor sUuid') aUuid')
    f _ = nout
    nout = ([], Nothing)

signOutCommand
    :: (Monad m) => (UuidFor SessionEvent) -> EventT SessionEvent m ()
signOutCommand sUuid' = do
    sessionState <- getState' initialSessionProjection sUuid'
    -- FIXME: technically can race if someone issues multiple times, but
    -- probably not worth the logic right now to fix.
    case ssStage sessionState of
      Active -> logEvents_' sUuid' AnyPosition
        [SessionSignedOutSessionEvent]
      _ -> return () -- Meh, it's ok to sign out repeatedly.

-- These functions provide a framework in which to run the above command
-- functions:

mkSessionCommandHandler
    :: (MonadIO m)
    => SenderAddress -> SmtpSettings -> LinkFormatter SessionEvent
    -> SessionCommand -> EventT SessionEvent m ()
mkSessionCommandHandler senderAddr smtpSettings fmtLink = sessionCommandHandler
  where
    sessionCommandHandler
      :: (MonadIO m) => SessionCommand -> EventT SessionEvent m ()
    sessionCommandHandler (RequestSessionSessionCommand _e) =
        -- Triggered directly by user and handled there:
        return ()
    sessionCommandHandler (BindSessionToAccountSessionCommand sUuid' aUuid') =
        bindSessionToAccountCommand (coerceUuidFor sUuid') aUuid'
    sessionCommandHandler (SendSignInEmailSessionCommand sUuid' e) =
        sendSignInEmailCommand senderAddr smtpSettings fmtLink
          (coerceUuidFor sUuid') e
    sessionCommandHandler (ExpireSessionRequestSessionCommand sUuid') =
        expireSessionRequestCommand (coerceUuidFor sUuid')
    sessionCommandHandler (SignInSessionCommand _sUuid' _uaString) =
        -- Triggered directly by user and handled there:
        return ()
    sessionCommandHandler (SignOutSessionCommand _sUuid') =
        -- Triggered directly by user and handled there:
        return ()


-- | An SessionUserActor wraps up the commands that users can submit
-- themselves (c.f. ones that happen automatically as a consequence of other
-- things happening)
data SessionUserActor
  = SessionUserActor
  { suaRequestSession :: EmailAddress -> IO ()
  , suaSignIn
      :: UuidFor SessionEvent -> UserAgentString -> IO (Maybe SessionCookie)
  , suaSignOut :: UuidFor SessionEvent -> IO ()
  }

newSessionUserActor
  :: IO UTCTime -> Store IO (TimeStamped Event) -> SessionUserActor
newSessionUserActor getT store = SessionUserActor
    (go . requestSessionCommand)
    (\sUuid' -> go . signInCommand sUuid')
    (go . signOutCommand)
  where
    go = sRunEventT store . timeStamp getT . liftSessionEventT

liftSessionEventT :: EventT SessionEvent IO a -> EventT Event IO a
liftSessionEventT = mapEvents toEvent eventToSessionEvent

eventToSessionEvent :: Event -> Maybe SessionEvent
eventToSessionEvent = decomposeEvent ignore ignore ignore Just
  where
    ignore = const Nothing
