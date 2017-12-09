{-# LANGUAGE OverloadedStrings #-}

module EmailAddress.CommandHandler where

import Data.Time.Clock (UTCTime)

import Eventful (ExpectedPosition(NoStream))

import Events
  ( TimeStamped, EmailAddress, Event, EmailAddressEvent(..), AccountEvent
  , toEvent)
import EventT (EventT, logEvents', logWithLatest_', timeStamp, mapEvents)
import Store (Store, sRunEventT)
import UuidFor (UuidFor)

import EmailAddress.Model
  ( initialEmailAddressProjection, EmailAddressState(..)
  , eventToEmailAddressEvent)


bindEmailToAcct
  :: (Monad m)
  => (EmailAddress -> UuidFor EmailAddressEvent)
  -> EmailAddress -> UuidFor (TimeStamped AccountEvent)
  -> EventT EmailAddressEvent m (Either String ())
bindEmailToAcct genUuid' e eaUuid' =
  -- FIXME: does this just need to take a UuidFor EmailAddressEvent?
  wrapError <$> logEvents' (genUuid' e) NoStream
    [EmailBoundToAccountEmailAddressEvent e eaUuid']
  where
    wrapError Nothing = Right ()
    wrapError _ = Left "Stream already exists"

removeEmail
  :: (Monad m)
  => UuidFor EmailAddressEvent -> EventT EmailAddressEvent m ()
removeEmail eaUuid' =
    logWithLatest_' initialEmailAddressProjection eaUuid' $
      \s -> case easEmailAddress s of
        "" -> []
        _ -> [EmailRemovedEmailAddressEvent]

data EmailAddressUserActor
  = EmailAddressUserActor
  { eauaBindEmailToAcct
      :: EmailAddress -> UuidFor (TimeStamped AccountEvent)
      -> IO (Either String ())
  , eauaRemoveEmail :: UuidFor EmailAddressEvent -> IO ()
  }

newEmailAddressUserActor
  :: IO UTCTime -> Store IO (TimeStamped Event)
  -> (EmailAddress -> UuidFor EmailAddressEvent) -> EmailAddressUserActor
newEmailAddressUserActor getT store genUuid' = EmailAddressUserActor
    (\e -> go . bindEmailToAcct genUuid' e)
    (go . removeEmail)
  where
    go = sRunEventT store . timeStamp getT . liftEmailAddressEventT

liftEmailAddressEventT :: EventT EmailAddressEvent IO a -> EventT Event IO a
liftEmailAddressEventT = mapEvents toEvent eventToEmailAddressEvent
