{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Events
  ( EmailAddress
  , RegistrationEmailType(..)
  , EmailEvent(..)
  , EmailAddressEvent(..)
  , AccountEvent(..)
  , UserAgentString(..)
  , SessionEvent(..)
  , Event(..)
  , emailEventToEvent
  , decomposeEvent
  , ToEvent, toEvent
  , liftProjection
  , TimeStamped, unTsUuidFor, tsUuidFor
  , EmailAddressCommand(..), AccountCommand(..), SessionCommand(..), Command(..)
  , decomposeCommand
  , ToCommand, toCommand
  ) where

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as T
import Data.DateTime (DateTime)
import Data.Text (Text)

import Eventful (Projection(..))

import UnionSums (unionSumTypes, mkConverter, mkDecompose)

import UuidFor (UuidFor, coerceUuidFor)


type TimeStamped a = (DateTime, a)

-- | A slightly more type-safe version of UuidFor coercion pertaining to time
-- stamps
unTsUuidFor :: UuidFor (TimeStamped event) -> UuidFor event
unTsUuidFor = coerceUuidFor

tsUuidFor :: UuidFor event -> UuidFor (TimeStamped event)
tsUuidFor = coerceUuidFor


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
  -- System-generated event:
  | EmailSentEmailEvent RegistrationEmailType
  deriving (Eq, Show)

data EmailAddressEvent
  = EmailBoundToAccountEmailAddressEvent
      EmailAddress (UuidFor (TimeStamped AccountEvent))
  | EmailRemovedEmailAddressEvent
  deriving (Eq, Show)

data AccountEvent
  = AccountCreatedAccountEvent
  | AccountNameUpdatedAccountEvent Text
  | ServiceUpdateEmailsEnabledAccountEvent
  | ServiceUpdateEmailsDisabledAccountEvent
  | AccountDeletedAccountEvent
  deriving (Eq, Show)

newtype UserAgentString =
    UserAgentString {unUserAgentString :: Text} deriving (Eq, Show)

instance ToJSON UserAgentString where
    toJSON = T.String . unUserAgentString

instance FromJSON UserAgentString where
    parseJSON = T.withText "UserAgentString" $ pure . UserAgentString


data SessionEvent
  = SessionRequestedSessionEvent EmailAddress
  | SessionBoundToAccountSessionEvent (UuidFor (TimeStamped AccountEvent))
  | SessionSignInEmailSentSessionEvent
  | SessionSignInEmailSendingFailedSessionEvent Text
  | SessionSignInWindowExpiredSessionEvent
  | SessionSignedInSessionEvent UserAgentString
  | SessionSignedOutSessionEvent
  deriving (Eq, Show)

unionSumTypes "Event"
  [''EmailEvent, ''EmailAddressEvent, ''AccountEvent, ''SessionEvent]
mkConverter ''EmailEvent ''Event
mkConverter ''EmailAddressEvent ''Event
mkConverter ''AccountEvent ''Event
mkConverter ''SessionEvent ''Event
mkDecompose ''Event
  [''EmailEvent, ''EmailAddressEvent, ''AccountEvent, ''SessionEvent]

deriving instance Eq Event
deriving instance Show Event


class ToEvent event where
  toEvent :: event -> Event

instance ToEvent EmailEvent where
  toEvent = emailEventToEvent

instance ToEvent EmailAddressEvent where
  toEvent = emailAddressEventToEvent

instance ToEvent AccountEvent where
  toEvent = accountEventToEvent

instance ToEvent SessionEvent where
  toEvent = sessionEventToEvent

instance ToEvent Event where
  toEvent = id

deriveJSON (aesonPrefix camelCase) ''RegistrationEmailType
deriveJSON (aesonPrefix camelCase) ''Event


liftProjection
  :: (event' -> Maybe event) -> Projection state event
  -> Projection state event'
liftProjection f (Projection seed handler) = Projection seed handler'
  where
    handler' state event' = maybe state (handler state) $ f event'


data EmailAddressCommand
  = BindEmailToAccountEmailAddressCommand
      EmailAddress (UuidFor (TimeStamped AccountEvent))
  | RemoveEmailEmailAddressCommand (UuidFor (TimeStamped EmailAddressEvent))
  deriving (Eq, Show)

data AccountCommand
  = EnsureAccountExistsForEmailAddrAccountCommand EmailAddress
  | UpdateAccountNameAccountCommand (UuidFor (TimeStamped AccountEvent)) Text
  | EnableServiceUpdateEmailsAccountCommand
  | DisableServiceUpdateEmailsAccountCommand
  deriving (Eq, Show)

data SessionCommand
  = RequestSessionSessionCommand EmailAddress
  | BindSessionToAccountSessionCommand
      (UuidFor (TimeStamped SessionEvent))
      (UuidFor (TimeStamped AccountEvent))
  | SendSignInEmailSessionCommand
      (UuidFor (TimeStamped SessionEvent))
      EmailAddress
  | ExpireSessionRequestSessionCommand (UuidFor (TimeStamped SessionEvent))
  | SignInSessionCommand (UuidFor (TimeStamped SessionEvent)) UserAgentString
  | SignOutSessionCommand (UuidFor (TimeStamped SessionEvent))
  deriving (Eq, Show)

unionSumTypes "Command"
  [''EmailAddressCommand, ''AccountCommand, ''SessionCommand]
mkConverter ''EmailAddressCommand ''Command
mkConverter ''AccountCommand ''Command
mkConverter ''SessionCommand ''Command
mkDecompose ''Command
  [''EmailAddressCommand, ''AccountCommand, ''SessionCommand]


class ToCommand command where
  toCommand :: command -> Command

instance ToCommand EmailAddressCommand where
  toCommand = emailAddressCommandToCommand

instance ToCommand AccountCommand where
  toCommand = accountCommandToCommand

instance ToCommand SessionCommand where
  toCommand = sessionCommandToCommand
