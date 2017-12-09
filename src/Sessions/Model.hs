{-# LANGUAGE OverloadedStrings #-}

module Sessions.Model where

import Data.Time.Clock (NominalDiffTime)

import Eventful (Projection(..))

import Events
  (EmailAddress, SessionEvent(..), AccountEvent, TimeStamped)
import Scheduler (mkTimeout)
import UuidFor (UuidFor(..))


sessionActivationTimeout :: NominalDiffTime
sessionActivationTimeout = mkTimeout $ 15 * 60

data SessionStage
  = Inactive
  | Pending
  | Expired -- User did not sign in before time window closed
  | Active
  | Terminated -- User was signed in and is now signed out
  deriving (Eq, Show)

data SessionState
  = SessionState
  { ssEmailAddress :: EmailAddress
  , ssStage :: SessionStage
  , ssAccountUuid' :: Maybe (UuidFor (TimeStamped AccountEvent))
  } deriving (Eq, Show)

initialSessionState :: SessionState
initialSessionState = SessionState "" Inactive Nothing

updateSessionState :: SessionState -> SessionEvent -> SessionState
updateSessionState
  (SessionState _ Inactive maUuid') (SessionRequestedSessionEvent e) =
  SessionState e Pending maUuid'
updateSessionState s (SessionSignInWindowExpiredSessionEvent) =
  s { ssStage = Expired }
updateSessionState s (SessionSignedInSessionEvent _) =
  s { ssStage = Active }
updateSessionState s (SessionSignedOutSessionEvent) =
  s { ssStage = Terminated }
updateSessionState s (SessionBoundToAccountSessionEvent aUuid') =
  s { ssAccountUuid' = Just aUuid' }
updateSessionState s _ = s

initialSessionProjection :: Projection SessionState SessionEvent
initialSessionProjection = Projection initialSessionState updateSessionState
