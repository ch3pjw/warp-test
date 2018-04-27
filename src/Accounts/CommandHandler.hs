{-# LANGUAGE ScopedTypeVariables #-}

module Accounts.CommandHandler where

import Control.Monad.IO.Class (MonadIO)

import Eventful (ExpectedPosition(..))

import Events
  ( EmailAddress, TimeStamped, Event(..), AccountEvent, EmailAddressEvent
  , liftProjection, AccountCommand(..))
import EventT (EventT, logEvents', logEvents_', getState')
import UuidFor (UuidFor)
import qualified UuidFor

-- import EmailAddress.Model
--   ( initialEmailAddressProjection
--   , eventToEmailAddressEvent, isUnassociated)


-- ensureAccountExistsForEmailAddr
--   :: (MonadIO m)
--   => (EmailAddress -> UuidFor (TimeStamped EmailAddressEvent))
--   -> EmailAddress
--   -> EventT Event m ()
-- ensureAccountExistsForEmailAddr genUuid' emailAddr =
--   let eUuid' = UuidFor.coerceUuidFor $ genUuid' emailAddr in do
--     emailState <- getState'
--         (liftProjection eventToEmailAddressEvent initialEmailAddressProjection)
--         (UuidFor.coerceUuidFor eUuid')
--     if isUnassociated emailState
--     then do
--       aUuid' <- newAcct
--       logEvents_' (UuidFor.coerceUuidFor eUuid') AnyPosition
--         [EmailBoundToAccountEvent emailAddr aUuid']
--     else return ()
--   where
--     newAcct = do
--       (aUuid' :: UuidFor (TimeStamped AccountEvent)) <- UuidFor.newRandom
--       maybeErr <- logEvents' (UuidFor.coerceUuidFor aUuid') NoStream
--           [AccountCreatedEvent]
--       case maybeErr of
--         Right _ -> return aUuid'
--         Left _ -> newAcct

-- mkAccountCommandHandler
--   :: (MonadIO m)
--   => (EmailAddress -> UuidFor (TimeStamped EmailAddressEvent))
--   -> AccountCommand -> EventT Event m ()
-- mkAccountCommandHandler genUuid' = acctCmdHandler
--   where
--     acctCmdHandler (EnsureAccountExistsForEmailAddrAccountCommand e) =
--       ensureAccountExistsForEmailAddr genUuid' e
--     acctCmdHandler _ = undefined
