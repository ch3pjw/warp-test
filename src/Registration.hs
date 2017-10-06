{-# LANGUAGE OverloadedStrings #-}

module Registration where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (void, forever)
import Data.DateTime (DateTime, getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (
  NominalDiffTime, secondsToDiffTime, diffUTCTime, addUTCTime)
import Data.UUID (UUID, nil)
import qualified Data.UUID as UUID

import Eventful (
  Projection(..), CommandHandler(..), getLatestStreamProjection,
  versionedStreamProjection, streamProjectionState, uuidFromInteger)
import Eventful.Store.Class (
  VersionedEventStoreWriter, VersionedEventStoreReader)
import Eventful.Store.Memory (
  eventMapTVar, tvarEventStoreWriter, tvarEventStoreReader,
  ExpectedPosition(..), storeEvents)


type EmailAddress = Text

type Timed a = (DateTime, a)

-- FIXME: from an environment variable or argument
verificationTimeout :: NominalDiffTime
verificationTimeout =
  fromRational . toRational $
  secondsToDiffTime $ 10 -- 60 * 60 * 24


data EmailType
  = VerificationEmail
  | ConfirmationEmail  -- Having clicked submit whilst verified
  | UnsubscribeEmail
  deriving (Eq, Show)

data VerificationState
  = Unverified
  | Pending DateTime
  | Verified
  deriving (Eq, Show)

data UserState
  = UserState
  { usVerificationState :: VerificationState
  , usPendingEmails :: [EmailType]
  , usEmailAddress :: EmailAddress
  } deriving (Eq, Show)


initialUserState = UserState Unverified [] ""

withinValidationPeriod :: DateTime -> UserState -> Bool
withinValidationPeriod now (UserState (Pending timeout) _ _ ) = now < timeout
withinValidationPeriod _ _ = False


condenseConsecutive :: (Eq a) => [a] -> [a]
condenseConsecutive [] = []
condenseConsecutive [a] = [a]
condenseConsecutive (a1:a2:as)
  | a1 == a2 = condenseConsecutive (a2:as)
  | otherwise = a1 : condenseConsecutive (a2:as)

data UserEvent
  -- User-generated events:
  = UserSubmitted EmailAddress
  | UserVerified
  | UserUnsubscribed
  -- System-generated events:
  | Emailed EmailType
  deriving (Eq, Show)

updateUserState :: UserState -> Timed UserEvent -> UserState
updateUserState (UserState vs es _) (t, UserSubmitted e)
  | vs == Verified = UserState
      Verified
      (es ++ [ConfirmationEmail])
      e
  | otherwise = UserState
      (Pending $ addUTCTime verificationTimeout t)
      (es ++ [VerificationEmail])
      e
updateUserState s (t, UserVerified) =
    s {usVerificationState = Verified}
updateUserState s@(UserState vs es _) (t, UserUnsubscribed) =
    s {usPendingEmails = es ++ [UnsubscribeEmail]}
updateUserState s (t, Emailed UnsubscribeEmail) =
    s {usVerificationState = Unverified
      , usPendingEmails = []
      , usEmailAddress = ""}
updateUserState s@(UserState _ es _) (t, Emailed emailType) =
    s {usPendingEmails = filter (/= emailType) es}


type UserProjection = Projection UserState (Timed UserEvent)

initialUserProjection = Projection initialUserState updateUserState


data UserCommand
  = Submit EmailAddress
  | Verify
  | Unsubscribe


handleUserCommand ::
  DateTime -> UserState -> UserCommand -> [Timed UserEvent]
-- Am I missing the point? This handler doesn't seem to do much. Most of the
-- logic is in the state machine defined by updateRegistrationState, and we
-- can't have any side effects here...
handleUserCommand now s (Submit e) = [(now, UserSubmitted e)]
handleUserCommand now s Verify =
  if withinValidationPeriod now s
  then [(now, UserVerified)]
  else []
handleUserCommand now s Unsubscribe =
  if not . Text.null $ usEmailAddress s
  then [(now, UserUnsubscribed)]
  else []


type UserCommandHandler =
  CommandHandler UserState (Timed UserEvent) UserCommand

userCommandHandler :: DateTime -> UserCommandHandler
userCommandHandler now =
  CommandHandler (handleUserCommand now) initialUserProjection


type Reader = VersionedEventStoreReader STM (Timed UserEvent)
type Writer = VersionedEventStoreWriter STM (Timed UserEvent)


setup :: IO (Writer, Reader)
setup = do
    tvar <- eventMapTVar
    let
      writer = tvarEventStoreWriter tvar
      reader = tvarEventStoreReader tvar
    return (writer, reader)


type DoAThing = (Writer, Reader) -> UUID -> IO ()


doThisThing :: UserCommand -> DoAThing
doThisThing cmd (w, r) uuid = do
    p <- atomically $ getLatestStreamProjection r $
      versionedStreamProjection uuid initialUserProjection
    now <- getCurrentTime
    let events = commandHandlerHandler
          (userCommandHandler now) (streamProjectionState p) cmd
    print events
    void . atomically $ storeEvents w uuid AnyPosition events


submitEmailAddress :: EmailAddress -> DoAThing
 -- FIXME: validate we got an actual email address
submitEmailAddress e (w, r) uuid =
  doThisThing (Submit e) (w, r) uuid >> putStrLn "Submitted" >> print uuid

verify :: DoAThing
verify = doThisThing Verify

unsubscribe :: DoAThing
unsubscribe = doThisThing Unsubscribe


getAndShowState :: DoAThing
getAndShowState (w, r) uuid = do
    p <- atomically $ getLatestStreamProjection r $
      versionedStreamProjection uuid initialUserProjection
    putStrLn . show $ streamProjectionState p


mockEmailToUuid :: EmailAddress -> UUID
mockEmailToUuid = uuidFromInteger . fromIntegral . Text.length


testLoop :: IO ()
testLoop = do
  (w, r) <- setup
  (i, o) <- U.newChan
  withAsync (emailer (w, r) o) $ \a -> forever $ do
    putStrLn "Command pls: s <email>, v <uuid>, u <uuid>"
    input <- getLine
    case input of
      's':' ':email -> let e = Text.pack email in
        submitEmailAddress e (w, r) (mockEmailToUuid e) >>
        U.writeChan i (mockEmailToUuid e)
      'v':' ':uuid -> parseUuidThen (\u -> verify (w, r) u >> U.writeChan i u) uuid
      'u':' ':uuid -> parseUuidThen (\u -> unsubscribe (w, r) u >> U.writeChan i u) uuid
      'g':' ':uuid -> parseUuidThen (getAndShowState (w, r)) uuid
      _ -> putStrLn "Narp, try again"
  where
    parseUuidThen f uuid = maybe (putStrLn "rubbish uuid") f $ UUID.fromString uuid


emailer :: (Writer, Reader) -> U.OutChan UUID -> IO ()
emailer (w, r) o = forever $ do
    uuid <- U.readChan o
    threadDelay 1000000
    p <- atomically $ getLatestStreamProjection r $
      versionedStreamProjection uuid initialUserProjection
    events <- sendEmails $ streamProjectionState p
    now <- getCurrentTime
    let events' = (,) now <$> events
    void . atomically $ storeEvents w uuid AnyPosition events'
  where
    sendEmails s =
      let emails = condenseConsecutive $ usPendingEmails s in
      putStrLn (show emails) >> return (Emailed <$> emails)
