{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Events
  ( UuidFor(..), coerceUuidFor
  , EmailAddress
  , RegistrationEmailType(..)
  , EmailEvent(..)
  , AccountEvent(..)
  , UserAgentString(..)
  , SessionEvent(..)
  , Event(..)
  , emailEventToEvent
  , decomposeEvent
  , ToEvent, toEvent
  , EventT, runEventT, logEvents, logEvents_, getState
  , mapEvents, liftToEvent
  , TimeStamped
  ) where

import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, withReaderT)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as T
import Data.DateTime (DateTime)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.UUID (UUID)

import Eventful (
    ExpectedPosition, EventStoreReader(..), EventWriteError, StreamEvent,
    getLatestStreamProjection, versionedStreamProjection,
    streamProjectionState, EventVersion, Projection)

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
  -- FIXME: dang it, I thought I changed this to "added" rather than
  --  "submitted" :-( When we next migrate the event log, let's change the name
  -- to that because it's better!
  = EmailAddressSubmittedEmailEvent EmailAddress
  | EmailAddressVerifiedEmailEvent
  | EmailAddressRemovedEmailEvent
  | EmailAddressAssociatedWithAccountEmailEvent (UuidFor AccountEvent)
  -- System-generated event:
  | EmailSentEmailEvent RegistrationEmailType
  | AssociationEmailSentEmailEvent
  deriving (Eq, Show)

data AccountEvent
  = AccountCreatedAccountEvent
  | AccountNameUpdatedAccountEvent Text
  deriving (Eq, Show)

newtype UserAgentString =
    UserAgentString {unUserAgentString :: Text} deriving (Eq, Show)

instance ToJSON UserAgentString where
    toJSON = T.String . unUserAgentString

instance FromJSON UserAgentString where
    parseJSON = T.withText "UserAgentString" $ pure . UserAgentString


data SessionEvent
  = SessionRequestedSessionEvent EmailAddress
  | SessionAssociatedWithAccountSessionEvent (UuidFor AccountEvent)
  | SessionSignInEmailSentSessionEvent
  | SessionSignedInSessionEvent UserAgentString
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


type StoreEvents key event m =
    key -> ExpectedPosition EventVersion -> [event]
    -> m (Maybe (EventWriteError EventVersion))

type EventT event state m
  = ReaderT
  ( EventStoreReader UUID EventVersion m (StreamEvent UUID EventVersion event)
  , StoreEvents UUID event m
  ) m

runEventT
    :: (Monad m)
    => EventT event state m a
    -> EventStoreReader UUID EventVersion m (StreamEvent UUID EventVersion event)
    -> StoreEvents UUID event m -> m a
runEventT et esr se = runReaderT et (esr, se)

logEvents
    :: (Monad m)
    => UUID -> ExpectedPosition EventVersion -> [event]
    -> EventT event state m (Maybe (EventWriteError EventVersion))
logEvents uuid pos events = ask >>= (\(_, se) -> lift $ se uuid pos events)

logEvents_
    :: (Monad m)
    => UUID -> ExpectedPosition EventVersion -> [event]
    -> EventT event state m ()
logEvents_ uuid pos = void . logEvents uuid pos

getState
    :: (Monad m)
    => Projection state event -> UUID -> EventT event state m state
getState proj key = ask >>= (\(reader, _) -> lift $
    streamProjectionState <$>
    getLatestStreamProjection reader (versionedStreamProjection key proj))

mapEvents
    :: (Monad m) => (event -> event') -> (event' -> Maybe event)
    -> EventT event state m a
    -> EventT event' state m a
mapEvents f f' et = withReaderT convert et
  where
    convert (reader, storeEvents) =
      (overReader reader, \k p -> storeEvents k p . fmap f)
    -- reader is a functor that internally deals in a list of events. We want to
    -- be able to drop events from that list if they're not pertinent to
    -- reconsituting our state, so we define the following:
    overReader (EventStoreReader reader) =
        EventStoreReader $ fmap (mapMaybe (sequence . fmap f')) <$> reader

liftToEvent
    :: (Monad m, ToEvent event)
    => (Event -> Maybe event)
    -> EventT event state m a
    -> EventT Event state m a
liftToEvent f' = mapEvents toEvent f'

type TimeStamped a = (DateTime, a)
