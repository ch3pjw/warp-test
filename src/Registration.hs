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
  , getAndShowState
  , EmailActor , newEmailActor
  , aPoll, aSubmitEmailAddress, aVerify, aUnsubscribe, aGetTime
  , reactivelyRunAction
  , timeStampedAction
  , getDatabaseConfig
  , untilNothing
  , RegistrationConfig, rcDatabaseConfig, rcUuidSalt
  , initialEmailProjection, liftProjection, liftAction
  , EmailStore
  , unsafeEventToEmailEvent
  ) where

import Prelude hiding (fail)
import Control.Exception (catch, SomeException)
import Control.Monad hiding (fail)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.IO.Class
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
import Data.UUID (UUID)
import qualified Data.UUID.V5 as UUIDv5
import System.Environment (getEnv)
import System.Envy (FromEnv, fromEnv, env)

import Database.Persist.URL (fromDatabaseUrl)
import qualified Database.Persist.Postgresql as DB

import Eventful (
  Projection(..), CommandHandler(..)
  )

import Events (
  EmailAddress, EmailType(..), EmailEvent(..), Event(..), emailEventToEvent,
  decomposeEvent)

import Store (Action, Store, sPoll, updateStore)


type Salt = Text

type TimeStamped a = (DateTime, a)

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


type EmailAction = Action EmailState (TimeStamped EmailEvent)

emailCommandAction :: IO DateTime -> EmailCommand -> EmailAction
emailCommandAction getT cmd = \s -> do
    t <- getT
    return $ commandHandlerHandler (userCommandHandler t) s cmd

timeStampedAction :: IO DateTime -> Action s e -> Action s (TimeStamped e)
timeStampedAction getT a = \s -> do
    t <- getT
    fmap ((,) t) <$> a s


data EmailActor
  = EmailActor
  { aGetTime :: IO DateTime
  , aSubmitEmailAddress :: EmailAddress -> IO ()
  , aVerify :: UUID -> IO ()
  , aUnsubscribe :: UUID -> IO ()
  , aPoll :: UUID -> IO EmailState
  }

newEmailActor :: Salt -> IO DateTime -> Store (TimeStamped Event) -> EmailActor
newEmailActor salt getT store = EmailActor
    getT
    (\e -> withUpdate (act $ Submit e) (hashEmail salt e))
    (\u -> withUpdate (act Verify) u)
    (\u -> withUpdate (act Unsubscribe) u)
    (sPoll store (liftProjection unsafeEventToEmailEvent initialEmailProjection))
  where
    act :: EmailCommand -> EmailAction
    act = emailCommandAction getT
    withUpdate :: EmailAction -> UUID -> IO ()
    withUpdate a u =
        liftedUpdateStore (fmap emailEventToEvent) unsafeEventToEmailEvent store
        initialEmailProjection a u

unsafeEventToEmailEvent :: TimeStamped Event -> TimeStamped EmailEvent
unsafeEventToEmailEvent = fmap $ decomposeEvent id (error "no!") (error "wrong!")

hashEmail :: Salt -> EmailAddress -> UUID
hashEmail salt email =
    UUIDv5.generateNamed UUIDv5.namespaceOID . BS.unpack . SHA256.hash $
    (Text.encodeUtf8 $ salt <> email)


getAndShowState
  :: (Show state) => Store event -> Projection state event -> UUID -> IO ()
getAndShowState p s = sPoll p s >=> print

untilNothing :: (MonadIO m) => IO (Maybe a) -> (a -> m ()) -> m ()
untilNothing wait f =
    liftIO wait >>=
    maybe (return ()) (\a -> f a >> untilNothing wait f)

reactivelyRunAction
  :: Projection state event -> (UUID -> Action state event) -> Store event
  -> IO (Maybe UUID) -> IO ()
reactivelyRunAction projection action store waitUuid = untilNothing waitUuid $
    -- We catch any exception the action raises because it shouldn't be able
    -- to bring down the entire reaction loop:
    \uuid ->
        updateStore projection (action uuid) store uuid
        `catch`
        -- FIXME: this should probably log
        (\(_ :: SomeException) -> return ())


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

liftAction
  :: (event -> event') -> Action state event -> Action state event'
liftAction f action = \state -> fmap f <$> action state

liftProjection
  :: (event' -> event) -> Projection state event -> Projection state event'
liftProjection f (Projection initialState updateState) =
    Projection initialState updateState'
  where
    updateState' state = updateState state . f

liftedUpdateStore
  :: (event -> event') -> (event' -> event) -> Store event'
  -> Projection state event -> Action state event -> UUID -> IO ()
liftedUpdateStore f f' s p a u =
    updateStore (liftProjection f' p) (liftAction f a) s u


deriveJSON (aesonPrefix camelCase) ''VerificationState
