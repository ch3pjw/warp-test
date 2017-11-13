{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Events
  ( EmailAddress
  , EmailType(..)
  , EmailEvent(..)
  , AccountEvent(..)
  , SessionEvent(..)
  , Event(..)
  , emailEventToEvent
  , decomposeEvent
  ) where

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as T
import Data.Text (Text)
import Data.UUID (UUID)

import UnionSums (unionSumTypes, mkConverter, mkDecompose)

type EmailAddress = Text

data EmailType
  = VerificationEmail
  | ConfirmationEmail  -- Having clicked submit whilst verified
  deriving (Eq, Show, Read)

deriveJSON (aesonPrefix camelCase) ''EmailType


data EmailEvent
  -- User-generated events:
  = EmailAddressSubmittedEmailEvent EmailAddress
  | EmailAddressVerifiedEmailEvent
  | EmailAddressRemovedEmailEvent
  -- System-generated event:
  | EmailSentEmailEvent EmailType
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

deriveJSON (aesonPrefix camelCase) ''Event
