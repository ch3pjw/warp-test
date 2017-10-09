{-# LANGUAGE OverloadedStrings #-}

module Registration where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Concurrent.STM (STM, atomically)
import Control.Monad
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

data Store = Store
  { _sReader :: Reader
  , _sWriter :: Writer
  , sGetNotificationChan :: IO (U.OutChan (Maybe UUID))
  , _sUpdate :: Action (TimeStamped UserEvent) -> UUID -> IO ()
  -- FIXME: probably for testing only?
  , sPoll :: UUID -> IO UserState
  }

newStore :: IO Store
newStore = do
    tvar <- eventMapTVar
    let
      w = tvarEventStoreWriter tvar
      r = tvarEventStoreReader tvar
    -- We assume that the unused OutChan gets cleaned up when it goes out of scope
    (i, _) <- U.newChan
    let
        update a uuid = do
            events <- getLatestState uuid >>= a uuid
            writeEvents w uuid events
            -- FIXME: we now can't "shut down" the store...
            when (not . null $ events) $ U.writeChan i (Just uuid)
        getLatestState uuid =
            streamProjectionState <$> getLatestUserProjection r uuid
    return $ Store r w (U.dupChan i) update getLatestState

updateStore :: Action (TimeStamped UserEvent) -> StoreUpdate
updateStore = flip _sUpdate


-- setup :: IO (Writer, Reader)
-- setup = do
--     tvar <- eventMapTVar
--     let
--       writer = tvarEventStoreWriter tvar
--       reader = tvarEventStoreReader tvar
--     return (writer, reader)


-- | An Action is a side-effect that runs on a particular stream's state and
-- | reports what it did as events
type Action e = UUID -> UserState -> IO [e]
type StoreUpdate = Store -> UUID -> IO ()

getLatestUserProjection r uuid = atomically $ getLatestStreamProjection r $
    versionedStreamProjection uuid initialUserProjection
writeEvents w uuid = void . atomically . storeEvents w uuid AnyPosition

commandAction :: UserCommand -> Action (TimeStamped UserEvent)
commandAction cmd = \_ s -> do
    t <- getCurrentTime
    return $ commandHandlerHandler (userCommandHandler t) s cmd

timeStampedAction :: IO DateTime -> Action e -> Action (TimeStamped e)
timeStampedAction getT a = \u s -> do
    t <- getT
    fmap ((,) t) <$> a u s

-- executeAction :: Action (TimeStamped UserEvent) -> StreamUpdate
-- executeAction a (w, r) uuid = getLatestState >>= a uuid >>= writeEvents w uuid
--   where
--     getLatestState = streamProjectionState <$> getLatestUserProjection r uuid


submitEmailAddress :: EmailAddress -> StoreUpdate
 -- FIXME: validate we got an actual email address
 -- MonadFail m => EmailAddress -> m StreamUpdate?
submitEmailAddress e = updateStore $ commandAction (Submit e)


verify :: StoreUpdate
verify = updateStore $ commandAction Verify

unsubscribe :: StoreUpdate
unsubscribe = updateStore $ commandAction Unsubscribe


getAndShowState :: StoreUpdate  -- Not really an "update"...
getAndShowState s = sPoll s >=> print


mockEmailToUuid :: EmailAddress -> UUID
mockEmailToUuid = uuidFromInteger . fromIntegral . Text.length


testLoop :: IO ()
testLoop = do
  store <- newStore
  o <- sGetNotificationChan store
  withAsync (reactivelyRunAction tsSendEmails store (U.readChan o)) $ \a -> forever $ do
    putStrLn "Command pls: s <email>, v <uuid>, u <uuid>"
    input <- getLine
    case input of
      's':' ':email -> let e = Text.pack email in
        submitEmailAddress e store (mockEmailToUuid e)
      'v':' ':uuid -> parseUuidThen (\u -> verify store u) uuid
      'u':' ':uuid -> parseUuidThen (\u -> unsubscribe store u) uuid
      'g':' ':uuid -> parseUuidThen (getAndShowState store) uuid
      _ -> putStrLn "Narp, try again"
  where
    parseUuidThen f uuid = maybe (putStrLn "rubbish uuid") f $ UUID.fromString uuid


reactivelyRunAction ::
    Action (TimeStamped UserEvent) -> Store -> IO (Maybe UUID) -> IO ()
reactivelyRunAction a store read =
    read >>= maybe (return ()) (
        \u -> updateStore a store u >> reactivelyRunAction a store read)


sendEmails :: Action UserEvent
sendEmails uuid s =
    let emails = condenseConsecutive $ usPendingEmails s in
    return $ Emailed <$> emails

tsSendEmails :: Action (TimeStamped UserEvent)
tsSendEmails = timeStampedAction getCurrentTime sendEmails
