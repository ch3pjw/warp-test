{-# LANGUAGE OverloadedStrings #-}

module EmailAddress.CommandHandler where

import Data.Time.Clock (UTCTime)

import Eventful (ExpectedPosition(NoStream), EventVersion)

import Events
  ( TimeStamped, EmailAddress, Event, EmailAddressEvent(..), AccountEvent
  , toEvent, tsUuidFor)
import EventT
  (EventT, logEvents', logWithLatest_', timeStamp, mapEvents, logNewStream')
import Store (Store, sRunEventT)
import UuidFor (UuidFor)

import EmailAddress.Model
  ( initialEmailAddressProjection, EmailAddressState(..)
  , eventToEmailAddressEvent)


-- bindEmailToAcct
--   :: (Monad m)
--   => EmailAddress -> UuidFor (TimeStamped AccountEvent)
--   -> EventT EmailAddressEvent m (UuidFor (TimeStamped EmailAddressEvent))
-- bindEmailToAcct e aUuid' =
--   -- FIXME: kinda want to wait on the log receiving an accepted or rejected
--   -- event before returning from this action...
--   tsUuidFor <$>
--     logNewStream' [BindEmailToAccountRequestedEmailAddressEvent e aUuid']

-- removeEmail
--   :: (Monad m)
--   => UuidFor EmailAddressEvent -> EventT EmailAddressEvent m EventVersion
-- removeEmail eaUuid' =
--     logWithLatest_' initialEmailAddressProjection eaUuid' $
--       \s -> case easEmailAddress s of
--         "" -> []
--         _ -> [EmailRemovedEmailAddressEvent]

-- data EmailAddressUserActor
--   = EmailAddressUserActor
--   { eauaBindEmailToAcct
--       :: EmailAddress -> UuidFor (TimeStamped AccountEvent)
--       -> IO (Either String ())
--   , eauaRemoveEmail :: UuidFor EmailAddressEvent -> IO EventVersion
--   }

-- newEmailAddressUserActor
--   :: IO UTCTime -> Store IO (TimeStamped Event)
--   -> (EmailAddress -> UuidFor EmailAddressEvent) -> EmailAddressUserActor
-- newEmailAddressUserActor getT store genUuid' = EmailAddressUserActor
--     (\e -> go . bindEmailToAcct genUuid' e)
--     (go . removeEmail)
--   where
--     go = sRunEventT store . timeStamp getT . liftEmailAddressEventT

-- liftEmailAddressEventT :: EventT EmailAddressEvent IO a -> EventT Event IO a
-- liftEmailAddressEventT = mapEvents toEvent eventToEmailAddressEvent
