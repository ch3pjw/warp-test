{-# LANGUAGE OverloadedStrings #-}

module Registration where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Concurrent.STM (STM, atomically)
import Control.Monad (void, forever, join)
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

type TimeStamped a = (DateTime, a)

-- FIXME: from an environment variable or argument
verificationTimeout :: NominalDiffTime
verificationTimeout =
  fromRational . toRational $
  secondsToDiffTime $ 10 -- 60 * 60 * 24


data EmailType
  = VerificationEmail
  | ConfirmationEmail  -- Having clicked submit whilst verified
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


-- | Converts neigbouring duplicates in a list into a single item
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

updateUserState :: UserState -> TimeStamped UserEvent -> UserState
updateUserState (UserState vs es _) (t, UserSubmitted e)
  | vs == Verified = UserState
      Verified
      (es ++ [ConfirmationEmail])
      e
  | otherwise = UserState
      (Pending $ addUTCTime verificationTimeout t)
      (es ++ [VerificationEmail])
      e
updateUserState s (t, UserVerified) = s {usVerificationState = Verified}
updateUserState s@(UserState vs es _) (t, UserUnsubscribed) = initialUserState
updateUserState s@(UserState _ es _) (t, Emailed emailType) =
    s {usPendingEmails = filter (/= emailType) es}


type UserProjection = Projection UserState (TimeStamped UserEvent)

initialUserProjection = Projection initialUserState updateUserState


data UserCommand
  = Submit EmailAddress
  | Verify
  | Unsubscribe


handleUserCommand ::
  DateTime -> UserState -> UserCommand -> [TimeStamped UserEvent]
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
  CommandHandler UserState (TimeStamped UserEvent) UserCommand

userCommandHandler :: DateTime -> UserCommandHandler
userCommandHandler now =
  CommandHandler (handleUserCommand now) initialUserProjection


type Reader = VersionedEventStoreReader STM (TimeStamped UserEvent)
type Writer = VersionedEventStoreWriter STM (TimeStamped UserEvent)


setup :: IO (Writer, Reader)
setup = do
    tvar <- eventMapTVar
    let
      writer = tvarEventStoreWriter tvar
      reader = tvarEventStoreReader tvar
    return (writer, reader)


type DoAThing = (Writer, Reader) -> UUID -> IO ()

getLatestUserProjection r uuid = atomically $ getLatestStreamProjection r $
    versionedStreamProjection uuid initialUserProjection
writeEvents w uuid = void . atomically . storeEvents w uuid AnyPosition


executeCommand :: UserCommand -> DoAThing
executeCommand cmd = runModification $ \s t ->
  return $ commandHandlerHandler (userCommandHandler t) s cmd


-- | An Action is a side-effect that reports what it did as events
type Action e = UserState -> IO [e]
type Modification = UserState -> DateTime -> IO [TimeStamped UserEvent]

timeStampedAction :: IO DateTime -> Action e -> Action (TimeStamped e)
timeStampedAction getT a = \s -> do
    t <- getT
    fmap ((,) t) <$> a s

executeAction :: (Writer, Reader) -> UUID -> Action (TimeStamped UserEvent) -> IO ()
executeAction (w, r) uuid a = getLatestState >>= a >>= writeEvents w uuid
  where
    getLatestState = streamProjectionState <$> getLatestUserProjection r uuid

runModification :: Modification -> DoAThing
runModification f (w, r) uuid = do
    events <- join $ f <$> getLatestState <*> getCurrentTime
    print events
    writeEvents w uuid events
  where
    getLatestState = streamProjectionState <$> getLatestUserProjection r uuid



submitEmailAddress :: EmailAddress -> DoAThing
 -- FIXME: validate we got an actual email address
submitEmailAddress e (w, r) uuid =
  executeCommand (Submit e) (w, r) uuid >> putStrLn "Submitted" >> print uuid

verify :: DoAThing
verify = executeCommand Verify

unsubscribe :: DoAThing
unsubscribe = executeCommand Unsubscribe


getAndShowState :: DoAThing
getAndShowState (w, r) uuid = do
    p <- getLatestUserProjection r uuid
    putStrLn . show $ streamProjectionState p


mockEmailToUuid :: EmailAddress -> UUID
mockEmailToUuid = uuidFromInteger . fromIntegral . Text.length


testLoop :: IO ()
testLoop = do
  (w, r) <- setup
  (i, o) <- U.newChan
  withAsync (reactivelyRunAction tsSendEmails (w, r) (U.readChan o)) $ \a -> forever $ do
    putStrLn "Command pls: s <email>, v <uuid>, u <uuid>"
    input <- getLine
    case input of
      's':' ':email -> let e = Text.pack email in
        submitEmailAddress e (w, r) (mockEmailToUuid e) >>
        U.writeChan i (Just $ mockEmailToUuid e)
      'v':' ':uuid -> parseUuidThen (\u -> verify (w, r) u >> U.writeChan i (Just u)) uuid
      'u':' ':uuid -> parseUuidThen (\u -> unsubscribe (w, r) u >> U.writeChan i (Just u)) uuid
      'g':' ':uuid -> parseUuidThen (getAndShowState (w, r)) uuid
      _ -> putStrLn "Narp, try again"
  where
    parseUuidThen f uuid = maybe (putStrLn "rubbish uuid") f $ UUID.fromString uuid


reactivelyRunAction ::
    Action (TimeStamped UserEvent) -> (Writer, Reader) -> IO (Maybe UUID) -> IO ()
reactivelyRunAction a (w, r) read =
    read >>= maybe (return ()) (\uuid ->
        executeAction (w, r) uuid a >> reactivelyRunAction a (w, r) read)


sendEmails :: Action UserEvent
sendEmails s =
    let emails = condenseConsecutive $ usPendingEmails s in
    do
    putStrLn (show emails)
    return $ Emailed <$> emails

tsSendEmails :: Action (TimeStamped UserEvent)
tsSendEmails = timeStampedAction getCurrentTime sendEmails
