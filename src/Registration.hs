{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Registration
  ( condenseConsecutive
  , EmailAddress
  , EmailType(..)
  , UserEvent(..)
  , TimeStamped
  , VerificationState(..), verificationTimeout
  , UserState, usEmailAddress, usPendingEmails, usVerificationState,
    initialUserState
  , Store, newStore, newDBStore, sGetNotificationChan, sSendShutdown, sPoll
  , getAndShowState
  , Actor, newActor, aSubmitEmailAddress, aVerify, aUnsubscribe, aGetTime
  , Action
  , reactivelyRunAction
  , timeStampedAction
  , getDatabaseConfig
  , untilNothing
  ) where

import qualified Control.Concurrent.Chan.Unagi as U
import Control.Concurrent.STM (atomically)
import Control.Exception (catch, SomeException)
import Control.Monad
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.ByteString as BS
import Data.DateTime (DateTime)
import Data.Monoid
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (NominalDiffTime, secondsToDiffTime, addUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID.V5 as UUIDv5
import System.Environment (getEnv)

import Database.Persist.URL (fromDatabaseUrl)
import qualified Database.Persist.Postgresql as DB
import Database.Persist.TH (derivePersistField)

import Eventful (
  Projection(..), CommandHandler(..), StreamProjection, EventVersion,
  getLatestStreamProjection, versionedStreamProjection, streamProjectionState)
import Eventful.Store.Memory (
  eventMapTVar, tvarEventStoreWriter, tvarEventStoreReader,
  ExpectedPosition(..), storeEvents)
import Eventful.Store.Postgresql (
  initializePostgresqlEventStore, defaultSqlEventStoreConfig,
  sqlEventStoreReader, postgresqlEventStoreWriter, jsonStringSerializer,
  serializedEventStoreWriter, serializedVersionedEventStoreReader)


type Salt = Text
type EmailAddress = Text

type TimeStamped a = (DateTime, a)

-- FIXME: from an environment variable or argument
verificationTimeout :: NominalDiffTime
verificationTimeout =
  fromRational . toRational $
  secondsToDiffTime $ 60 * 60 * 24


data EmailType
  = VerificationEmail
  | ConfirmationEmail  -- Having clicked submit whilst verified
  deriving (Eq, Show, Read)
derivePersistField "EmailType"

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


initialUserState :: UserState
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
updateUserState s (_, UserVerified) = s {usVerificationState = Verified}
updateUserState (UserState _ _ _) (_, UserUnsubscribed) = initialUserState
updateUserState s@(UserState _ es _) (_, Emailed emailType) =
    s {usPendingEmails = filter (/= emailType) es}


type UserProjection = Projection UserState (TimeStamped UserEvent)

initialUserProjection :: UserProjection
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
handleUserCommand now _ (Submit e) = [(now, UserSubmitted e)]
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


data Store = Store
  { sGetNotificationChan :: IO (U.OutChan (Maybe UUID))
  -- FIXME: sendShutdown feels weird, because it doesn't mean anything to
  -- actually writing to the store...
  , sSendShutdown :: IO ()
  , _sUpdate :: Action (TimeStamped UserEvent) -> UUID -> IO ()
  -- FIXME: probably for testing only?
  , sPoll :: UUID -> IO UserState
  }

newStoreFrom
  :: (UUID -> [TimeStamped UserEvent] -> IO ())
  -> (UUID -> IO (
         StreamProjection UUID EventVersion UserState (TimeStamped UserEvent))
     )
  -> IO Store
newStoreFrom write getLatestUserProjection = do
    -- We assume that the unused OutChan gets cleaned up when it goes out of
    -- scope:
    (i, _) <- U.newChan
    let update a uuid = do
            events <- getLatestState uuid >>= a uuid
            write uuid events
            when (not . null $ events) $ U.writeChan i (Just uuid)
        getLatestState uuid =
            streamProjectionState <$> getLatestUserProjection uuid
    return $
      Store (U.dupChan i) (U.writeChan i Nothing) update getLatestState

newStore :: IO Store
newStore = do
    tvar <- eventMapTVar
    let
      w = tvarEventStoreWriter tvar
      r = tvarEventStoreReader tvar
    -- FIXME: AnyPosition always valid?
    newStoreFrom
      (\uuid -> void . atomically . storeEvents w uuid AnyPosition)
      (\uuid -> atomically $ getLatestStreamProjection r $
          versionedStreamProjection uuid initialUserProjection)

updateStore :: Action (TimeStamped UserEvent) -> Store -> UUID -> IO ()
updateStore = flip _sUpdate


-- | An Action is a side-effect that runs on a particular stream's state and
-- | reports what it did as events
type Action e = UUID -> UserState -> IO [e]

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

untilNothing :: (MonadIO m) => IO (Maybe a) -> (a -> m ()) -> m ()
untilNothing wait f =
    liftIO wait >>=
    maybe (return ()) (\a -> f a >> untilNothing wait f)

reactivelyRunAction ::
    Action (TimeStamped UserEvent) -> Store -> IO (Maybe UUID) -> IO ()
reactivelyRunAction a store waitUuid = untilNothing waitUuid (
    -- We catch any exception the action raises because it shouldn't be able
    -- to bring down the entire reaction loop:
    \u -> updateStore a store u `catch` (\(_ :: SomeException) -> return ()))


getDatabaseConfig :: IO DB.PostgresConf
getDatabaseConfig = join $ fromDatabaseUrl 1 <$> getEnv "DATABASE_URL"


deriveJSON (aesonPrefix camelCase) ''EmailType
deriveJSON (aesonPrefix camelCase) ''VerificationState
deriveJSON (aesonPrefix camelCase) ''UserEvent


newDBStore :: Pool DB.SqlBackend -> IO Store
newDBStore pool =
  let
    writer = serializedEventStoreWriter jsonStringSerializer $
        postgresqlEventStoreWriter defaultSqlEventStoreConfig
    reader = serializedVersionedEventStoreReader jsonStringSerializer $
        sqlEventStoreReader defaultSqlEventStoreConfig
  in do
    initializePostgresqlEventStore pool
    newStoreFrom
      (\uuid events -> void $
          DB.runSqlPool (storeEvents writer uuid AnyPosition events) pool)
      (\uuid -> DB.runSqlPool
          (getLatestStreamProjection reader $
              versionedStreamProjection uuid initialUserProjection) pool)
