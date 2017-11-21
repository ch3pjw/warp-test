{-# LANGUAGE OverloadedStrings #-}

module Session where

import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.DateTime (DateTime)
import Data.Time.Clock (NominalDiffTime, secondsToDiffTime, addUTCTime)
import Data.UUID.V4 (nextRandom)

import Eventful (ExpectedPosition(..), Projection(..))

import Events
  ( EmailAddress, SessionEvent(..), EventT, logEvents
  , logEvents_, getState, UuidFor(..), UserAgentString, TimeStamped)


sessionActivationTimeout :: NominalDiffTime
sessionActivationTimeout =
  fromRational . toRational $
  secondsToDiffTime $ 15 * 60

data SessionStage
  = Inactive
  | Pending DateTime
  | Active
  | Terminated
  deriving (Eq, Show)

data SessionState
  = SessionState
  { ssEmailAddress :: EmailAddress
  , ssStage :: SessionStage
  } deriving (Eq, Show)

withinActivationPeriod :: DateTime -> SessionState -> Bool
withinActivationPeriod now (SessionState _ (Pending timeout)) = now < timeout
withinActivationPeriod _ _ = False

initialSessionState :: SessionState
initialSessionState = SessionState "" Inactive

updateSessionState :: SessionState -> TimeStamped SessionEvent -> SessionState
updateSessionState
  (SessionState _ Inactive) (t, SessionRequestedSessionEvent e) =
  SessionState e (Pending $ addUTCTime sessionActivationTimeout t)
updateSessionState s (_, SessionSignedInSessionEvent _) =
  s { ssStage = Active }
updateSessionState s (_, SessionSignedOutSessionEvent) =
  s { ssStage = Terminated }
updateSessionState s _ = s

initialSessionProjection :: Projection SessionState (TimeStamped SessionEvent)
initialSessionProjection = Projection initialSessionState updateSessionState

type SessionEventT = EventT (TimeStamped SessionEvent) IO

data SessionActor
  = SessionActor
  { saRequestSession :: EmailAddress -> SessionEventT (UuidFor SessionEvent)
  , saSignIn
      :: UuidFor SessionEvent -> UserAgentString
      -> SessionEventT (Either String ())
  , saSignOut :: UuidFor SessionEvent -> SessionEventT ()
  }

newSessionActor :: IO DateTime -> SessionActor
newSessionActor getT = SessionActor requestSession signIn signOut
  where
    requestSession emailAddr = do
        t <- liftIO getT
        uuid' <- newSessionUuid'
        -- Logging events can fail if the stream happens to exist (collision),
        -- so we retry:
        void $ untilJust $ logEvents (unUuidFor uuid') NoStream
          [(t, SessionRequestedSessionEvent emailAddr)]
        return uuid'
    signIn uuid' uaString = do
        t <- liftIO getT
        sessionState <- getState initialSessionProjection (unUuidFor uuid')
        if withinActivationPeriod t sessionState
          then do
              logEvents_ (unUuidFor uuid') AnyPosition
                [(t, SessionSignedInSessionEvent uaString)]
              return $ pure ()
          else
              return $ fail "Not within validation period"
    signOut uuid' = do
        t <- liftIO getT
        -- We query the existing session state so that malicious clients can't
        -- note their sign-out link and keep spamming us with sign-out requests
        -- to fill up the DB:
        sessionState <- getState initialSessionProjection (unUuidFor uuid')
        when (ssStage sessionState /= Terminated) $
          logEvents_ (unUuidFor uuid') AnyPosition
            [(t, SessionSignedOutSessionEvent)]

untilJust :: (Monad m) => m (Maybe a) -> m a
untilJust m = m >>= maybe (untilJust m) return

newSessionUuid' :: (MonadIO m) => m (UuidFor SessionEvent)
newSessionUuid' = UuidFor <$> liftIO nextRandom
