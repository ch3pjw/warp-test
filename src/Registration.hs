{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Registration
  ( condenseConsecutive
  , EmailAddress
  , EmailType(..)
  , UserEvent(..)
  , TimeStamped
  , VerificationState(..), verificationTimeout
  , EmailState, usEmailAddress, usPendingEmails, usVerificationState,
    initialEmailState
  , Store, newStore, newDBStore, sGetNotificationChan, sSendShutdown, sPoll
  , getAndShowState
  , Actor, newActor, aSubmitEmailAddress, aVerify, aUnsubscribe, aGetTime
  , Action
  , reactivelyRunAction
  , timeStampedAction
  , getDatabaseConfig
  , untilNothing
  , RegistrationConfig, rcDatabaseConfig, rcUuidSalt
  , initialEmailProjection
  , EmailStore
  ) where

import Prelude hiding (fail)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Concurrent.STM (atomically)
import Control.Exception (catch, SomeException)
import Control.Monad hiding (fail)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (ToJSON, FromJSON)
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
import System.Envy (FromEnv, fromEnv, env)

import Database.Persist.URL (fromDatabaseUrl)
import qualified Database.Persist.Postgresql as DB

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

data VerificationState
  = Unverified
  | Pending DateTime
  | Verified
  deriving (Eq, Show)

data EmailState
  = EmailState
  { usVerificationState :: VerificationState
  , usPendingEmails :: [EmailType]
  , usEmailAddress :: EmailAddress
  } deriving (Eq, Show)


initialEmailState :: EmailState
initialEmailState = EmailState Unverified [] ""

withinValidationPeriod :: DateTime -> EmailState -> Bool
withinValidationPeriod now (EmailState (Pending timeout) _ _ ) = now < timeout
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

updateEmailState :: EmailState -> TimeStamped UserEvent -> EmailState
updateEmailState (EmailState vs es _) (t, UserSubmitted e)
  | vs == Verified = EmailState
      Verified
      (es ++ [ConfirmationEmail])
      e
  | otherwise = EmailState
      (Pending $ addUTCTime verificationTimeout t)
      (es ++ [VerificationEmail])
      e
updateEmailState s (_, UserVerified) = s {usVerificationState = Verified}
updateEmailState (EmailState _ _ _) (_, UserUnsubscribed) = initialEmailState
updateEmailState s@(EmailState _ es _) (_, Emailed emailType) =
    s {usPendingEmails = filter (/= emailType) es}


type EmailProjection = Projection EmailState (TimeStamped UserEvent)

initialEmailProjection :: EmailProjection
initialEmailProjection = Projection initialEmailState updateEmailState


data EmailCommand
  = Submit EmailAddress
  | Verify
  | Unsubscribe
  deriving (Show, Eq)


handleEmailCommand ::
  DateTime -> EmailState -> EmailCommand -> [TimeStamped UserEvent]
-- Am I missing the point? This handler doesn't seem to do much. Most of the
-- logic is in the state machine defined by updateRegistrationState, and we
-- can't have any side effects here...
handleEmailCommand now _ (Submit e) = [(now, UserSubmitted e)]
handleEmailCommand now s Verify =
  if withinValidationPeriod now s
  then [(now, UserVerified)]
  else []
handleEmailCommand now s Unsubscribe =
  if not . Text.null $ usEmailAddress s
  then [(now, UserUnsubscribed)]
  else []


type EmailCommandHandler =
  CommandHandler EmailState (TimeStamped UserEvent) EmailCommand

userCommandHandler :: DateTime -> EmailCommandHandler
userCommandHandler now =
  CommandHandler (handleEmailCommand now) initialEmailProjection


data Store state event = Store
  { sGetNotificationChan :: IO (U.OutChan (Maybe UUID))
  -- FIXME: sendShutdown feels weird, because it doesn't mean anything to
  -- actually writing to the store...
  , sSendShutdown :: IO ()
  , _sUpdate :: Action state event -> UUID -> IO ()
  -- FIXME: probably for testing only?
  , sPoll :: UUID -> IO state
  }

newStoreFrom
  :: (UUID -> [event] -> IO ())
  -> (UUID -> IO (StreamProjection UUID EventVersion state event))
  -> IO (Store state event)
newStoreFrom write getLatestProjection = do
    -- We assume that the unused OutChan gets cleaned up when it goes out of
    -- scope:
    (i, _) <- U.newChan
    let update a uuid = do
            events <- getLatestState uuid >>= a uuid
            write uuid events
            when (not . null $ events) $ U.writeChan i (Just uuid)
        getLatestState uuid = streamProjectionState <$> getLatestProjection uuid
    return $
      Store (U.dupChan i) (U.writeChan i Nothing) update getLatestState

newStore :: Projection state event -> IO (Store state event)
newStore initialProjection = do
    tvar <- eventMapTVar
    let
      w = tvarEventStoreWriter tvar
      r = tvarEventStoreReader tvar
    -- FIXME: AnyPosition always valid?
    newStoreFrom
      (\uuid -> void . atomically . storeEvents w uuid AnyPosition)
      (\uuid -> atomically $ getLatestStreamProjection r $
          versionedStreamProjection uuid initialProjection)

updateStore :: Action state event -> Store state event -> UUID -> IO ()
updateStore = flip _sUpdate


-- | An Action is a side-effect that runs on a particular stream's state and
-- | reports what it did as events
type Action state event = UUID -> state -> IO [event]

commandAction :: EmailCommand -> DateTime -> Action EmailState (TimeStamped UserEvent)
commandAction cmd t = \_ s -> do
    return $ commandHandlerHandler (userCommandHandler t) s cmd

timeStampedAction :: IO DateTime -> Action s e -> Action s (TimeStamped e)
timeStampedAction getT a = \u s -> do
    t <- getT
    fmap ((,) t) <$> a u s

type EmailStore = Store EmailState (TimeStamped UserEvent)


data Actor
  = Actor
  { aGetTime :: IO DateTime
  , aSubmitEmailAddress :: EmailStore -> EmailAddress -> IO ()
  , aVerify :: EmailStore -> UUID -> IO ()
  , aUnsubscribe :: EmailStore -> UUID -> IO ()
  }

newActor :: Salt -> IO DateTime -> Actor
newActor salt getT = Actor
    getT
    (\s e -> wrap (submitEmailAddress e) s ())
    (wrap verify)
    (wrap unsubscribe)
  where
    wrap f s u = getT >>= \t -> f t s u

    submitEmailAddress :: EmailAddress -> DateTime -> EmailStore -> a -> IO ()
    submitEmailAddress e t s _ = updateStore
        (commandAction (Submit e) t)
        s
        (hashEmail salt e)

    verify :: DateTime -> EmailStore -> UUID -> IO ()
    verify = updateStore . commandAction Verify

    unsubscribe :: DateTime -> EmailStore -> UUID -> IO ()
    unsubscribe = updateStore . commandAction Unsubscribe


hashEmail :: Salt -> EmailAddress -> UUID
hashEmail salt email =
    UUIDv5.generateNamed UUIDv5.namespaceOID . BS.unpack . SHA256.hash $
    (Text.encodeUtf8 $ salt <> email)


getAndShowState :: (Show state) => Store state event -> UUID -> IO ()
getAndShowState s = sPoll s >=> print

untilNothing :: (MonadIO m) => IO (Maybe a) -> (a -> m ()) -> m ()
untilNothing wait f =
    liftIO wait >>=
    maybe (return ()) (\a -> f a >> untilNothing wait f)

reactivelyRunAction ::
    Action state event -> Store state event -> IO (Maybe UUID) -> IO ()
reactivelyRunAction a store waitUuid = untilNothing waitUuid (
    -- We catch any exception the action raises because it shouldn't be able
    -- to bring down the entire reaction loop:
    \u -> updateStore a store u `catch` (\(_ :: SomeException) -> return ()))


newtype CanFail a = CanFail
  { unCanFail :: Either String a
  } deriving (Functor, Applicative, Monad, Show)

instance MonadFail CanFail where
  fail = CanFail . Left

-- | Helper to turn a MonadFail into a ExceptT/MonadError
f2e :: (MonadError String m) => CanFail a -> m a
f2e = either throwError return . unCanFail


data RegistrationConfig = RegistrationConfig
  { rcDatabaseConfig :: DB.PostgresConf
  , rcUuidSalt :: Text
  } deriving (Show)

instance FromEnv RegistrationConfig where
    fromEnv = RegistrationConfig
        <$> (
          env "DATABASE_URL" >>=
          f2e . \(s :: String) -> fromDatabaseUrl 1 s)
        <*> env "REGISTRATION_UUID_SALT"


getDatabaseConfig :: IO DB.PostgresConf
getDatabaseConfig = join $ fromDatabaseUrl 1 <$> getEnv "DATABASE_URL"


deriveJSON (aesonPrefix camelCase) ''EmailType
deriveJSON (aesonPrefix camelCase) ''VerificationState
deriveJSON (aesonPrefix camelCase) ''UserEvent


newDBStore
  :: (ToJSON event, FromJSON event)
  =>  Projection state event -> Pool DB.SqlBackend -> IO (Store state event)
newDBStore initialProjection pool =
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
              versionedStreamProjection uuid initialProjection) pool)
