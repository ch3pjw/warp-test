{-# LANGUAGE OverloadedStrings #-}

module Registration where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Concurrent.STM (STM, atomically)
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.DateTime (DateTime, getCurrentTime)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (
  NominalDiffTime, secondsToDiffTime, diffUTCTime, addUTCTime)
import Data.UUID (UUID, nil)
import qualified Data.UUID as UUID
import qualified Data.UUID.V5 as UUIDv5

import Eventful (
  Projection(..), CommandHandler(..), getLatestStreamProjection,
  versionedStreamProjection, streamProjectionState, uuidFromInteger)
import Eventful.Store.Class (
  VersionedEventStoreWriter, VersionedEventStoreReader)
import Eventful.Store.Memory (
  eventMapTVar, tvarEventStoreWriter, tvarEventStoreReader,
  ExpectedPosition(..), storeEvents)


type Salt = Text
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
  deriving (Show, Eq)


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
  -- FIXME: sendShutdown feels weird, because it doesn't mean anything to
  -- actually writing to the store...
  , sSendShutdown :: IO ()
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
    return $
      Store r w (U.dupChan i) (U.writeChan i Nothing) update getLatestState

updateStore :: Action (TimeStamped UserEvent) -> Store -> UUID -> IO ()
updateStore = flip _sUpdate


-- | An Action is a side-effect that runs on a particular stream's state and
-- | reports what it did as events
type Action e = UUID -> UserState -> IO [e]

getLatestUserProjection r uuid = atomically $ getLatestStreamProjection r $
    versionedStreamProjection uuid initialUserProjection
writeEvents w uuid = void . atomically . storeEvents w uuid AnyPosition

commandAction :: UserCommand -> DateTime -> Action (TimeStamped UserEvent)
commandAction cmd t = \_ s -> do
    return $ commandHandlerHandler (userCommandHandler t) s cmd

timeStampedAction :: IO DateTime -> Action e -> Action (TimeStamped e)
timeStampedAction getT a = \u s -> do
    t <- getT
    fmap ((,) t) <$> a u s


data Actor
  = Actor
  { aGetTime :: IO DateTime
  , aSubmitEmailAddress :: Store -> EmailAddress -> IO ()
  , aVerify :: Store -> UUID -> IO ()
  , aUnsubscribe :: Store -> UUID -> IO ()
  }

newActor :: Salt -> IO DateTime -> Actor
newActor salt getT = Actor
    getT
    (\s e -> wrap (submitEmailAddress e) s ())
    (wrap verify)
    (wrap unsubscribe)
  where
    wrap f s u = getT >>= \t -> f t s u

    submitEmailAddress :: EmailAddress -> DateTime -> Store -> a -> IO ()
    submitEmailAddress e t s _ = updateStore
        (commandAction (Submit e) t)
        s
        (hashEmail salt e)

    verify :: DateTime -> Store -> UUID -> IO ()
    verify = updateStore . commandAction Verify

    unsubscribe :: DateTime -> Store -> UUID -> IO ()
    unsubscribe = updateStore . commandAction Unsubscribe


hashEmail :: Salt -> EmailAddress -> UUID
hashEmail salt email =
    UUIDv5.generateNamed UUIDv5.namespaceOID . BS.unpack . SHA256.hash $
    (Text.encodeUtf8 $ salt <> email)


getAndShowState :: Store -> UUID -> IO ()
getAndShowState s = sPoll s >=> print


mockEmailToUuid :: EmailAddress -> UUID
mockEmailToUuid = uuidFromInteger . fromIntegral . Text.length


testLoop :: IO ()
testLoop = do
  store <- newStore
  o <- sGetNotificationChan store
  let actor = newActor "NaCl" getCurrentTime
  go store actor o
  where
    parseUuidThen f uuid = maybe (putStrLn "rubbish uuid") f $ UUID.fromString uuid
    go store actor o =
        withAsync (reactivelyRunAction tsSendEmails store (U.readChan o)) $
          \a -> do
            putStrLn "Command pls: s <email>, v <uuid>, u <uuid>, g <uuid>, q"
            input <- getLine
            case input of
              's':' ':email -> let e = Text.pack email in
                  aSubmitEmailAddress actor store e >> go store actor o
              'v':' ':uuid ->
                  parseUuidThen (\u -> aVerify actor store u) uuid >>
                  go store actor o
              'u':' ':uuid ->
                  parseUuidThen (\u -> aUnsubscribe actor store u) uuid >>
                  go store actor o
              'g':' ':uuid -> parseUuidThen (getAndShowState store) uuid
              'q':_ -> sSendShutdown store
              _ -> putStrLn "Narp, try again" >> go store actor o


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
