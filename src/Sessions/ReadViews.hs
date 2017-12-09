{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Sessions.ReadViews where

import Control.Monad (void)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)
import Data.Text (Text)
import Database.Persist.Postgresql ((=.), (==.), (+=.))
import qualified Database.Persist.Postgresql as DB
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import Eventful.Store.Postgresql () -- For UUID postgresification

import Events
  ( EmailAddress, UserAgentString(..), Event, SessionEvent(..), AccountEvent
  , EmailAddressEvent(..), TimeStamped, decomposeEvent, Command(..))
import ReadView
  (SimpleRvUpdate, simpleProcessManager, ProcessManager(..))
import Scheduler (Scheduled, actNow, actAfter, actBefore, actBetween, mkTimeout)
import UuidFor (UuidFor, coerceUuidFor)

import Sessions.Model (sessionActivationTimeout)


-- | How long it is until we missed the opportunity to send a requested sign in
-- email
emailSendTimeout :: NominalDiffTime
emailSendTimeout = mkTimeout $ 60 * 60


share [mkPersist sqlSettings, mkMigrate "migrateSPM"] [persistLowerCase|
SessionPMActiveSession
    sessionUuid (UuidFor (TimeStamped SessionEvent))
    accountUuid (UuidFor (TimeStamped AccountEvent))
    userAgent Text
    UniqueSessionPMActiveSessionUuid sessionUuid
    deriving Show

SessionPMPendingSession
    sessionUuid (UuidFor (TimeStamped SessionEvent))
    emailAddress EmailAddress
    accountUuid (UuidFor (TimeStamped AccountEvent)) Maybe
    sendEmailBy UTCTime
    emailSendAttempts Int
    UniqueSessionPMPendingSessionSessionUuid sessionUuid
    deriving Show

SessionPMEmailAcctAssoc
    emailAddress EmailAddress
    emailUuid (UuidFor (TimeStamped EmailAddressEvent))
    accountUuid (UuidFor (TimeStamped AccountEvent))
    UniqueSessionPMEmailAcctAssocEmailAddr emailAddress
    UniqueSessionPMEmailAcctAssocEmailUuid emailUuid
    deriving Show
|]

updateActiveSession
  :: SimpleRvUpdate (TimeStamped SessionEvent) [Scheduled Command]
updateActiveSession sUuid' (t, SessionRequestedSessionEvent e) = do
    void $ DB.insertBy $ SessionPMPendingSession sUuid' e Nothing expiryTime 0
    maybeAssoc <- DB.getBy $ UniqueSessionPMEmailAcctAssocEmailAddr e
    return $ maybe [actNow $ EnsureAccountExistsForEmailAddrCommand e]
        ( pure . actNow . BindSessionToAccountCommand sUuid'
          . sessionPMEmailAcctAssocAccountUuid . DB.entityVal
        )
        maybeAssoc
  where
    expiryTime = addUTCTime emailSendTimeout t
updateActiveSession
  sUuid' (_t, SessionBoundToAccountSessionEvent aUuid') = do
    DB.updateWhere
      [SessionPMPendingSessionSessionUuid ==. sUuid']
      [SessionPMPendingSessionAccountUuid =. Just aUuid']
    maybePending <- DB.getBy $ UniqueSessionPMPendingSessionSessionUuid sUuid'
    return $ genCmd maybePending
  where
    genCmd Nothing = []
    genCmd (Just pendingEntity) =
      let
        pending = DB.entityVal pendingEntity
        expiryTime = sessionPMPendingSessionSendEmailBy pending
        emailAddr = sessionPMPendingSessionEmailAddress pending
      in
        [actBefore expiryTime $ SendSignInEmailCommand sUuid' emailAddr]
updateActiveSession uuid' (_t, SessionSignedInSessionEvent uaString) = do
    -- FIXME: when this can't find the pending session, don't just ignore!
    pending <- fmap DB.entityVal <$>
      DB.getBy (UniqueSessionPMPendingSessionSessionUuid uuid')
    maybe (return ()) (void . DB.insertBy) (pending >>= fromPendingSession)
    DB.deleteBy $ UniqueSessionPMPendingSessionSessionUuid uuid'
    return []
  where
    fromPendingSession
      :: SessionPMPendingSession -> Maybe SessionPMActiveSession
    fromPendingSession (SessionPMPendingSession sUuid' _ (Just aUuid') _ _) =
        return $
          SessionPMActiveSession sUuid' aUuid' (unUserAgentString uaString)
    fromPendingSession _ = Nothing
updateActiveSession uuid' (_t, SessionSignedOutSessionEvent) = do
    DB.deleteBy $ UniqueSessionPMActiveSessionUuid uuid'
    return []
updateActiveSession uuid' (t, SessionSignInEmailSentSessionEvent) = do
    DB.deleteBy $ UniqueSessionPMPendingSessionSessionUuid uuid'
    return [actAfter expiryTime $ ExpireSessionRequestCommand uuid']
  where
    expiryTime = addUTCTime sessionActivationTimeout t
updateActiveSession
  uuid' (t, SessionSignInEmailSendingFailedSessionEvent _) = do
    maybePending <- DB.getBy $ UniqueSessionPMPendingSessionSessionUuid uuid'
    DB.updateWhere
      [SessionPMPendingSessionSessionUuid ==. uuid']
      [SessionPMPendingSessionEmailSendAttempts +=. 1]
    return $ genCmd maybePending
  where
    genCmd
      :: Maybe (DB.Entity SessionPMPendingSession) -> [Scheduled Command]
    genCmd Nothing = []
    genCmd (Just pendingEntity) =
      let
        pending = DB.entityVal pendingEntity
        backoffWait = mkTimeout $
          (2 ^ sessionPMPendingSessionEmailSendAttempts pending) * 30
        backoffUntil = addUTCTime backoffWait t
        expiryTime = sessionPMPendingSessionSendEmailBy pending
      in
        maybe [] pure $ actBetween backoffUntil expiryTime
          $ SendSignInEmailCommand uuid'
          $ sessionPMPendingSessionEmailAddress pending
updateActiveSession uuid' (_t, SessionSignInWindowExpiredSessionEvent) = do
    DB.deleteBy $ UniqueSessionPMPendingSessionSessionUuid uuid'
    return []

updateAccountAssociation
  :: SimpleRvUpdate (TimeStamped EmailAddressEvent) [Command]
updateAccountAssociation
  eUuid' (_t, EmailBoundToAccountEmailAddressEvent e aUuid') = do
    void $ DB.insertBy $ SessionPMEmailAcctAssoc e eUuid' aUuid'
    sessionUuids <-
        fmap (sessionPMPendingSessionSessionUuid . DB.entityVal) <$>
        DB.selectList
          [ SessionPMPendingSessionEmailAddress ==. e
          , SessionPMPendingSessionAccountUuid ==. Nothing] []
    return $
        fmap (flip BindSessionToAccountCommand aUuid') sessionUuids
updateAccountAssociation eUuid' (_t, EmailRemovedEmailAddressEvent) = do
    DB.deleteBy $ UniqueSessionPMEmailAcctAssocEmailUuid eUuid'
    return []

activeSessionProcessManager :: ProcessManager (TimeStamped Event) (Scheduled Command)
activeSessionProcessManager =
    simpleProcessManager "session_process_manager" migrateSPM update poll
  where
    update uuid' event =
        let t = fst event in
        decomposeEvent
          noop
          (\ee -> fmap actNow <$> updateAccountAssociation (coerceUuidFor uuid') (t, ee))
          noop
          (\se -> updateActiveSession (coerceUuidFor uuid') (t, se))
          (snd event)
    noop = const $ return []
    poll = undefined
