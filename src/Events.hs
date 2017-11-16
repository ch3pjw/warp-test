{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Events
  ( UuidFor(..), coerceUuidFor
  , EmailAddress
  , RegistrationEmailType(..)
  , EmailEvent(..)
  , AccountEvent(..)
  , SessionEvent(..)
  , Event(..), liftToEvent
  , emailEventToEvent
  , decomposeEvent
  , EventT, logEvents
  , TimeStamped, timeStamp
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Writer (WriterT(..), writer, runWriterT)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as T
import Data.DateTime (DateTime)
import Data.Text (Text)
import Data.UUID (UUID)

import UnionSums (unionSumTypes, mkConverter, mkDecompose)

newtype UuidFor event = UuidFor {unUuidFor :: UUID} deriving (Eq, Show)

coerceUuidFor :: UuidFor event -> UuidFor event'
coerceUuidFor = UuidFor . unUuidFor

instance ToJSON (UuidFor event) where
  toJSON = toJSON . unUuidFor

instance FromJSON (UuidFor event) where
  parseJSON = fmap UuidFor . parseJSON

type EmailAddress = Text

data RegistrationEmailType
  = VerificationEmail
  | ConfirmationEmail  -- Having clicked submit whilst verified
  deriving (Eq, Show, Read)

data EmailEvent
  -- User-generated events:
  = EmailAddressSubmittedEmailEvent EmailAddress
  | EmailAddressVerifiedEmailEvent
  | EmailAddressRemovedEmailEvent
  -- System-generated event:
  | EmailSentEmailEvent RegistrationEmailType
  deriving (Eq, Show)

data AccountEvent
  = AccountNameUpdatedAccountEvent Text
  | AccountAddedEmailAddressAccountEvent UUID
  | AccountRemovedEmailAddressAccountEvent UUID
  | AccountAddedSessionAccountEvent UUID
  | AccountRemovedSessionAccountEvent UUID
  deriving (Eq, Show)

newtype UserAgentString =
    UserAgentString {unUserAgentString :: Text} deriving (Eq, Show)

instance ToJSON UserAgentString where
    toJSON = T.String . unUserAgentString

instance FromJSON UserAgentString where
    parseJSON = T.withText "UserAgentString" $ pure . UserAgentString

data SessionEvent
  = SessionSignedInSessionEvent UserAgentString
  | SessionSignedOutSessionEvent
  deriving (Eq, Show)

unionSumTypes "Event" [''EmailEvent, ''AccountEvent, ''SessionEvent]
mkConverter ''EmailEvent ''Event
mkConverter ''AccountEvent ''Event
mkConverter ''SessionEvent ''Event
mkDecompose ''Event [''EmailEvent, ''AccountEvent, ''SessionEvent]

deriving instance Eq Event
deriving instance Show Event


class ToEvent event where
  toEvent :: event -> Event

instance ToEvent EmailEvent where
  toEvent = emailEventToEvent

instance ToEvent AccountEvent where
  toEvent = accountEventToEvent

instance ToEvent SessionEvent where
  toEvent = sessionEventToEvent

instance ToEvent Event where
  toEvent = id

deriveJSON (aesonPrefix camelCase) ''RegistrationEmailType
deriveJSON (aesonPrefix camelCase) ''Event

type EventT event m a = WriterT [event] m a

logEvents :: (Monad m) => [event] -> EventT event m ()
logEvents es = writer ((), es)

liftToEvent :: (Monad m, ToEvent event) => EventT event m a -> EventT Event m a
liftToEvent et = WriterT $ do
    (a, es) <- runWriterT et
    return (a, toEvent <$> es)

type TimeStamped a = (DateTime, a)

timeStamp
  :: (MonadIO m)
  => (IO DateTime)
  -> EventT event m a
  -> EventT (TimeStamped event) m a
timeStamp getTime et = WriterT $ do
    (a , es) <- runWriterT et
    t <- liftIO getTime
    return (a, (,) t <$> es)
