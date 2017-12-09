{-# LANGUAGE OverloadedStrings #-}

module EmailAddress.Model where

import qualified Data.UUID as UUID

import Eventful (Projection(..))

import Events
  ( TimeStamped, EmailAddress, Event, decomposeEvent, EmailAddressEvent(..)
  , AccountEvent)
import UuidFor (UuidFor(..))

data EmailAddressState
  = EmailAddressState
  { easEmailAddress :: EmailAddress
  , easAccountUuid' :: UuidFor (TimeStamped AccountEvent)
  } deriving (Eq, Show)

initialEmailAddressState :: EmailAddressState
initialEmailAddressState = EmailAddressState "" (UuidFor UUID.nil)

isUnassociated :: EmailAddressState -> Bool
isUnassociated = UUID.null . unUuidFor . easAccountUuid'

updateEmailAddressState
  :: EmailAddressState -> EmailAddressEvent -> EmailAddressState
updateEmailAddressState _ (EmailBoundToAccountEmailAddressEvent e aUuid') =
  EmailAddressState e aUuid'
updateEmailAddressState _ EmailRemovedEmailAddressEvent =
  initialEmailAddressState

initialEmailAddressProjection :: Projection EmailAddressState EmailAddressEvent
initialEmailAddressProjection =
  Projection initialEmailAddressState updateEmailAddressState

eventToEmailAddressEvent :: Event -> Maybe EmailAddressEvent
eventToEmailAddressEvent = decomposeEvent ignore Just ignore ignore
  where
    ignore = const Nothing
