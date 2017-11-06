{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Events where

import Data.Text (Text)
import Data.UUID (UUID)
import Data.ByteString (ByteString)

import TH (mashSumTypes)
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

type UserAgentString = ByteString

data SessionEvent
  = SessionSignedInSessionEvent UserAgentString
  | SessionSignedOutSessionEvent
  deriving (Eq, Show)

mashSumTypes "Event" [''EmailEvent, ''AccountEvent, ''SessionEvent]

deriving instance Eq Event
deriving instance Show Event
