{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Registration
  ( condenseConsecutive
  , TimeStamped
  , VerificationState(..), verificationTimeout
  , EmailState, esEmailAddress, esPendingEmails, esVerificationState,
    initialEmailState
  , EmailActor , newEmailActor
  , aPoll, aSubmitEmailAddress, aVerify, aUnsubscribe, aGetTime
  , reactivelyRunEventTWithState
  , getDatabaseConfig
  , untilNothing
  , RegistrationConfig, rcDatabaseConfig, rcUuidSalt
  , initialEmailProjection, liftProjection, liftEventT
  , EmailStore
  , unsafeEventToEmailEvent
  ) where

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer (WriterT(..))
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.ByteString as BS
import Data.DateTime (DateTime)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (NominalDiffTime, secondsToDiffTime, addUTCTime)
import qualified Data.UUID.V5 as UUIDv5
import System.Environment (getEnv)
import System.Envy (FromEnv, fromEnv, env)

import Database.Persist.URL (fromDatabaseUrl)
import qualified Database.Persist.Postgresql as DB

import Eventful (
  Projection(..), CommandHandler(..)
  )

import Events (
  EmailAddress, RegistrationEmailType(..), EmailEvent(..), UuidFor(..),
  coerceUuidFor, Event(..), emailEventToEvent, decomposeEvent, TimeStamped,
  EventT, logEvents)

import Store (Store, sRunEventTWithState)


type Salt = Text

-- FIXME: from an environment variable or argument
verificationTimeout :: NominalDiffTime
verificationTimeout =
  fromRational . toRational $
  secondsToDiffTime $ 60 * 60 * 24

data VerificationState
  = Unverified
  | Pending DateTime
  | Verified
  deriving (Eq, Show)

data EmailState
  = EmailState
  { esVerificationState :: VerificationState
  , esPendingEmails :: [RegistrationEmailType]
  , esEmailAddress :: EmailAddress
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

updateEmailState :: EmailState -> TimeStamped EmailEvent -> EmailState
updateEmailState (EmailState vs es _) (t, EmailAddressSubmittedEmailEvent e)
  | vs == Verified = EmailState
      Verified
      (es ++ [ConfirmationEmail])
      e
  | otherwise = EmailState
      (Pending $ addUTCTime verificationTimeout t)
      (es ++ [VerificationEmail])
      e
updateEmailState s (_, EmailAddressVerifiedEmailEvent) =
    s {esVerificationState = Verified}
updateEmailState _ (_, EmailAddressRemovedEmailEvent) = initialEmailState
updateEmailState s@(EmailState _ es _) (_, EmailSentEmailEvent emailType) =
    s {esPendingEmails = filter (/= emailType) es}


type EmailProjection = Projection EmailState (TimeStamped EmailEvent)

initialEmailProjection :: EmailProjection
initialEmailProjection = Projection initialEmailState updateEmailState


data EmailCommand
  = Submit EmailAddress
  | Verify
  | Unsubscribe
  deriving (Show, Eq)


handleEmailCommand ::
  DateTime -> EmailState -> EmailCommand -> [TimeStamped EmailEvent]
-- Am I missing the point? This handler doesn't seem to do much. Most of the
-- logic is in the state machine defined by updateRegistrationState, and we
-- can't have any side effects here...
handleEmailCommand now _ (Submit e) = [(now, EmailAddressSubmittedEmailEvent e)]
handleEmailCommand now s Verify =
  if withinValidationPeriod now s
  then [(now, EmailAddressVerifiedEmailEvent)]
  else []
handleEmailCommand now s Unsubscribe =
  if not . Text.null $ esEmailAddress s
  then [(now, EmailAddressRemovedEmailEvent)]
  else []


type EmailCommandHandler =
  CommandHandler EmailState (TimeStamped EmailEvent) EmailCommand

userCommandHandler :: DateTime -> EmailCommandHandler
userCommandHandler now =
  CommandHandler (handleEmailCommand now) initialEmailProjection


type EmailStore = Store (TimeStamped EmailEvent)


type EmailAction m a = EmailState -> EventT (TimeStamped EmailEvent) m a

emailCommandAction
  :: (MonadIO m) => IO DateTime -> EmailCommand -> EmailAction m ()
emailCommandAction getT cmd = \s -> do
    t <- liftIO getT
    logEvents $ commandHandlerHandler (userCommandHandler t) s cmd


data EmailActor
  = EmailActor
  { aGetTime :: IO DateTime
  , aSubmitEmailAddress :: EmailAddress -> IO ()
  , aVerify :: UuidFor EmailEvent -> IO ()
  , aUnsubscribe :: UuidFor EmailEvent -> IO ()
  , aPoll :: UuidFor EmailEvent -> IO EmailState
  }

newEmailActor :: Salt -> IO DateTime -> Store (TimeStamped Event) -> EmailActor
newEmailActor salt getT store = EmailActor
    getT
    (\e -> withUpdate (act $ Submit e) (hashEmail salt e))
    (\u -> withUpdate (act Verify) u)
    (\u -> withUpdate (act Unsubscribe) u)
    (\u -> sRunEventTWithState store
        (liftProjection unsafeEventToEmailEvent initialEmailProjection)
        return
        (coerceUuidFor u))
  where
    act :: EmailCommand -> EmailAction IO ()
    act = emailCommandAction getT
    withUpdate :: EmailAction IO () -> UuidFor EmailEvent -> IO ()
    withUpdate a u =
        liftedStoreRunEventTWithState (fmap emailEventToEvent)
        unsafeEventToEmailEvent store initialEmailProjection a (coerceUuidFor u)

unsafeEventToEmailEvent :: TimeStamped Event -> TimeStamped EmailEvent
unsafeEventToEmailEvent = fmap $ decomposeEvent id (error "no!") (error "wrong!")

hashEmail :: Salt -> EmailAddress -> UuidFor EmailEvent
hashEmail salt email =
    UuidFor .
    UUIDv5.generateNamed UUIDv5.namespaceOID . BS.unpack . SHA256.hash $
    (Text.encodeUtf8 $ salt <> email)

untilNothing :: (MonadIO m) => IO (Maybe a) -> (a -> m ()) -> m ()
untilNothing wait f =
    liftIO wait >>=
    maybe (return ()) (\a -> f a >> untilNothing wait f)

reactivelyRunEventTWithState
  :: (MonadIO m)
  => Projection state event -> (UuidFor event -> state -> EventT event m ())
  -> (IO (Maybe (UuidFor event))) -> Store event -> m ()
reactivelyRunEventTWithState projection f waitUuid' store = do
    untilNothing (waitUuid') $ \uuid' ->
      sRunEventTWithState store projection (f uuid') uuid'


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

-- | Intended to help lift a store that acts on a subset of the global set of
--   events into the global space. In this case event is the "smaller" event
--   type and event' the "bigger".

liftEventT
  :: (Monad m) => (event -> event') -> EventT event m a -> EventT event' m a
liftEventT f eventT = WriterT $ do
    (result, events) <- runWriterT eventT
    return (result, f <$> events)

liftProjection
  :: (event' -> event) -> Projection state event -> Projection state event'
liftProjection f (Projection initialState updateState) =
    Projection initialState updateState'
  where
    updateState' state = updateState state . f

liftedStoreRunEventTWithState
  :: (MonadIO m)
  => (event -> event') -> (event' -> event) -> Store event'
  -> Projection state event -> (state -> EventT event m a) -> UuidFor event'
  -> m a
liftedStoreRunEventTWithState f f' s p eventT uuid' =
    sRunEventTWithState s (liftProjection f' p) (liftEventT f . eventT) uuid'


deriveJSON (aesonPrefix camelCase) ''VerificationState
