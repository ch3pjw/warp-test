{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Events where

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as T
import Data.Text (Text)
import Data.UUID (UUID)
import Data.ByteString (ByteString)

import TH (unionSumTypes)
import Registration (EmailAddress, EmailType)

data EmailEvent
  = EmailAddressSubmittedEmailEvent EmailAddress
  | EmailAddressVerifiedEmailEvent
  | EmailAddressRemovedEmailEvent
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

deriving instance Eq Event
deriving instance Show Event

deriveJSON (aesonPrefix camelCase) ''Event
