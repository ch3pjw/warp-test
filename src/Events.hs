{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Events
  ( EmailAddress
  , RegistrationEmailType(..)
  , EmailEvent(..)
  , AccountEvent(..)
  , UserAgentString(..)
  , SessionEvent(..)
  , Event(..)
  , emailEventToEvent
  , decomposeEvent
  , ToEvent, toEvent
  , TimeStamped
  ) where

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as T
import Data.DateTime (DateTime)
import Data.Text (Text)

import UnionSums (unionSumTypes, mkConverter, mkDecompose)

import UuidFor (UuidFor)



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

type TimeStamped a = (DateTime, a)
