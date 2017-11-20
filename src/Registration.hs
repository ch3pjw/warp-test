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
  , initialEmailProjection
  , EmailStore
  , unsafeEventToEmailEvent, slightlySaferEventToEmailEvent
  ) where

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.ByteString as BS
import Data.DateTime (DateTime)
import Data.Functor.Contravariant (contramap)
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
  Projection(..), ExpectedPosition(AnyPosition)
  )

import Events (
  EmailAddress, RegistrationEmailType(..), EmailEvent(..), UuidFor(..),
  Event(..), decomposeEvent, TimeStamped,
  EventT, logEvents', getState, mapEvents, toEvent)

import Store (Store, sRunEventT)



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

type EmailStore = Store IO (TimeStamped EmailEvent)

type EmailAction m a = EventT (TimeStamped EmailEvent) EmailState m a


data EmailActor
  = EmailActor
  { aGetTime :: IO DateTime
  , aSubmitEmailAddress :: EmailAddress -> IO ()
  , aVerify :: UuidFor EmailEvent -> IO VerificationState
  , aUnsubscribe :: UuidFor EmailEvent -> IO ()
  , aPoll :: UuidFor EmailEvent -> IO EmailState
  }

newEmailActor
    :: Salt -> IO DateTime -> Store IO (TimeStamped Event) -> EmailActor
newEmailActor salt getT store = EmailActor
    getT
    (go . submitEmail)
    (go . verifyEmail)
    (go . unsubEmail)
    (go . getState . unUuidFor)
  where
    go = sRunEventT store initP . liftEventT
    initP = contramap unsafeEventToEmailEvent initialEmailProjection
    liftEventT = mapEvents (fmap toEvent) slightlySaferEventToEmailEvent
    submitEmail :: (MonadIO m) => EmailAddress -> EmailAction m ()
    submitEmail e = do
        t <- liftIO getT
        logEvents' (unUuidFor $ hashEmail salt e) AnyPosition
            [(t, EmailAddressSubmittedEmailEvent e)]
    verifyEmail
        :: (MonadIO m)
        => UuidFor EmailEvent -> EmailAction m VerificationState
    verifyEmail uuid' =
      let uuid = unUuidFor uuid' in do
        t <- liftIO getT
        emailState <- getState uuid
        if withinValidationPeriod t emailState
          then
            logEvents' uuid AnyPosition [(t, EmailAddressVerifiedEmailEvent)]
            >> return Verified
          else
            return $ esVerificationState emailState
    unsubEmail :: (MonadIO m) => UuidFor EmailEvent -> EmailAction m ()
    unsubEmail uuid' =
      let uuid = unUuidFor uuid' in do
        t <- liftIO getT
        emailState <- getState uuid
        when (not . Text.null $ esEmailAddress emailState) $
          logEvents' uuid AnyPosition [(t, EmailAddressRemovedEmailEvent)]

unsafeEventToEmailEvent :: TimeStamped Event -> TimeStamped EmailEvent
unsafeEventToEmailEvent = fmap $ decomposeEvent id (error "no!") (error "wrong!")

slightlySaferEventToEmailEvent
    :: TimeStamped Event -> Maybe (TimeStamped EmailEvent)
slightlySaferEventToEmailEvent =
    traverse $ decomposeEvent Just (const Nothing) (const Nothing)

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
  => Projection state event
  -> (x -> EventT event state m ())
  -> IO (Maybe x) -> Store m event -> m ()
reactivelyRunEventTWithState projection f waitX store = do
    untilNothing waitX $ \x -> sRunEventT store projection (f x)


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

deriveJSON (aesonPrefix camelCase) ''VerificationState
