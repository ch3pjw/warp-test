module Registration where

import Control.Concurrent.STM (STM, atomically)
import Control.Monad (void)
import Data.DateTime (DateTime, getCurrentTime)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, secondsToDiffTime, diffUTCTime)
import Data.UUID (UUID, nil)

import Eventful (
  Projection(..), CommandHandler(..), getLatestStreamProjection,
  versionedStreamProjection, streamProjectionState)
import Eventful.Store.Class (
  VersionedEventStoreWriter, VersionedEventStoreReader)
import Eventful.Store.Memory (
  eventMapTVar, tvarEventStoreWriter, tvarEventStoreReader,
  ExpectedPosition(..), storeEvents)


type EmailAddress = Text

verificationTimeout :: NominalDiffTime
verificationTimeout =
  fromRational . toRational $
  secondsToDiffTime $ 60 * 60 * 24

data RegistrationState
  = HasNotRegistered
  | HasSubmittedEmail DateTime
  | HasBeenEmailed DateTime
  | HasClickedVerify
  | HasResubmittedEmail  -- whilst verified
  | HasUnsubscribed
  deriving (Show, Eq)

initialRegistrationState = HasNotRegistered


withinValidationPeriod :: DateTime -> RegistrationState -> Bool
withinValidationPeriod now (HasBeenEmailed t) =
  diffUTCTime now t < verificationTimeout
withinValidationPeriod _ _ = False


data RegistrationEvent
  = Submitted EmailAddress DateTime
  | WasEmailedVerificationLink DateTime
  | Verified DateTime
  | VerificationLapsed DateTime
  | WasEmailedVerificationConfirmation DateTime
  | Unsubscribed DateTime
  deriving (Eq, Show)


updateRegistrationState ::
  (EmailAddress, RegistrationState) ->
  RegistrationEvent -> (EmailAddress, RegistrationState)
--
updateRegistrationState (_, HasNotRegistered) (Submitted e t) =
  (e, HasSubmittedEmail t)
--
updateRegistrationState (_, HasSubmittedEmail _) (Submitted e t) =
  (e, HasSubmittedEmail t)
updateRegistrationState (e, HasSubmittedEmail _) (WasEmailedVerificationLink t) =
  (e, HasBeenEmailed t)
updateRegistrationState (e, HasSubmittedEmail _) (Verified t) =
  (e, HasClickedVerify)
updateRegistrationState (e, HasSubmittedEmail _) (VerificationLapsed t) =
  (mempty, HasUnsubscribed)
--
updateRegistrationState (_, HasBeenEmailed _) (Submitted e t) =
  (e, HasSubmittedEmail t)
updateRegistrationState (e, HasBeenEmailed _) (Verified t) =
  (e, HasClickedVerify)
updateRegistrationState (e, HasBeenEmailed _) (VerificationLapsed t) =
  (mempty, HasUnsubscribed)
--
updateRegistrationState (_, HasClickedVerify) (Submitted e t) =
  (e, HasResubmittedEmail)
updateRegistrationState (e, HasClickedVerify) (Verified t) =
  (e, HasClickedVerify)
--
updateRegistrationState (_, HasResubmittedEmail) (Submitted e t) =
  (e, HasResubmittedEmail)
updateRegistrationState (e, HasResubmittedEmail) (Verified t) =
  (e, HasResubmittedEmail)
updateRegistrationState (e, HasResubmittedEmail) (WasEmailedVerificationLink t) =
  (e, HasClickedVerify)
--
updateRegistrationState (_, HasUnsubscribed) (Submitted e t) =
  (e, HasSubmittedEmail t)
-- Catch-alls:
updateRegistrationState _ (Unsubscribed t) = (mempty, HasUnsubscribed)
updateRegistrationState s (WasEmailedVerificationLink t) = s
updateRegistrationState _ _ = undefined


type RegistrationProjection =
  Projection (EmailAddress, RegistrationState) RegistrationEvent

initialRegistrationProjection =
  Projection (mempty, initialRegistrationState) updateRegistrationState


data RegistrationCommand
  = Submit EmailAddress
  | RecordEmailSent  -- This feels weird
  | Verify
  | Unsubscribe


handleRegistrationCommand ::
  DateTime -> (EmailAddress, RegistrationState) -> RegistrationCommand ->
  [RegistrationEvent]
-- Am I missing the point? This handler doesn't seem to do much. Most of the
-- logic is in the state machine defined by updateRegistrationState, and we
-- can't have any side effects here...
handleRegistrationCommand now (_, s) (Submit e) = [Submitted e now]
handleRegistrationCommand now (e, s) RecordEmailSent = [WasEmailedVerificationLink now]
handleRegistrationCommand now (e, s) Verify =
  if withinValidationPeriod now s
  then [Verified now]
  else [VerificationLapsed now]
handleRegistrationCommand now (e, s) Unsubscribe = [Unsubscribed now]


type RegistrationCommandHandler =
  CommandHandler
    (EmailAddress, RegistrationState)
    RegistrationEvent
    RegistrationCommand

registrationCommandHandler :: DateTime -> RegistrationCommandHandler
registrationCommandHandler now =
  CommandHandler (handleRegistrationCommand now) initialRegistrationProjection


type Reader = VersionedEventStoreReader STM RegistrationEvent
type Writer = VersionedEventStoreWriter STM RegistrationEvent


setup :: IO (Writer, Reader)
setup = do
    tvar <- eventMapTVar
    let
      writer = tvarEventStoreWriter tvar
      reader = tvarEventStoreReader tvar
    return (writer, reader)


submitEmailAddress ::
  (Writer, Reader) -> (EmailAddress -> UUID) -> EmailAddress -> IO ()
submitEmailAddress (w, r) toUuid e = do
    p <- atomically $ getLatestStreamProjection r $
      versionedStreamProjection uuid initialRegistrationProjection
    now <- getCurrentTime
    let events = commandHandlerHandler (registrationCommandHandler now) (streamProjectionState p)
    store w uuid events
  where
    uuid = toUuid e


recordEmailSent = undefined

verify :: (Writer, Reader) -> UUID -> IO ()
verify uuid = do
  latest <- getLatestState r uuid
  now <- getCurrentTime
  let events = commandHandlerHandler (Registration

unsubscribe = undefined


mockEmailToUuid = undefined
