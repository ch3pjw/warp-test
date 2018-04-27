{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module EmailAddress.ReadViews where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Pool (Pool)
import qualified  Database.Persist.Postgresql as DB
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import Eventful (SequenceNumber, EventVersion)

import Events
  ( EmailAddress, TimeStamped, Event, AccountEvent
  , AccountEvent(..)
  , EmailAddressEvent(..), Command(..), decomposeEvent, tsUuidFor)
import EventT (EventT, logNewStream)
import ReadView (IsView(..))
import UuidFor (UuidFor, coerceUuidFor)


share [mkPersist sqlSettings, mkMigrate "migrateAEPM"] [persistLowerCase|
AcctEmailPMPendingAcct sql=account_emails_pm_pending_accounts
    accountUuid (UuidFor (TimeStamped AccountEvent))
    emailAddress EmailAddress
    UniqueAcctEmailPMPendingAcctAcctUuid accountUuid
    UniqueAcctEmailPMPendingAcctEmailAddress emailAddress
    deriving Show

AcctEmailPMPendingAssoc sql=account_email_pm_pending_assocs
    emailAddress EmailAddress
    emailUuid (UuidFor (TimeStamped EmailAddressEvent))
    accountUuid (UuidFor (TimeStamped AccountEvent))
    UniqueAcctEmailPMPendingAssocEmailAddress emailAddress
    UniqueAcctEmailPMPendingAssocEmailUuid emailUuid
    deriving Show

AcctEmailPMAssoc sql=account_emails_pm_email_account_assocs
    emailAddress EmailAddress
    emailUuid (UuidFor (TimeStamped EmailAddressEvent))
    accountUuid (UuidFor (TimeStamped AccountEvent))
    UniqueAcctEmailPMAssocEmailUuid emailUuid
    UniqueAcctEmailPMAssocEmailAddr emailAddress
    deriving Show
|]

trackAccounts
  :: (MonadIO m) => UuidFor AccountEvent -> AccountEvent
  -> ReaderT DB.SqlBackend m [Command]
trackAccounts aUuid' (AccountCreationRequestedForEmailAccountEvent e) = do
  mAssocEnt <- DB.getBy $ UniqueAcctEmailPMAssocEmailAddr e
  case mAssocEnt of
    Nothing ->
      (DB.insertBy $ AcctEmailPMPendingAcct (tsUuidFor aUuid') e) >>= either
        (const $
         return [RejectAccountCreationRequestCommand $ tsUuidFor aUuid'])
        (const $
         return [AcceptAccountCreationRequestCommand $ tsUuidFor aUuid'])
    _ -> return [RejectAccountCreationRequestCommand $ tsUuidFor aUuid']
trackAccounts aUuid' AccountCreationRejectedDuplicateAccountEvent =
  -- In the case of rejection, there will be another entry with the
  -- corresponding email address in the table already, which will be removed
  -- when that event is accepted. So, we do nothing here.
  return []
trackAccounts aUuid' AccountCreationAcceptedAccountEvent = do
  pending <- fmap DB.entityVal <$>
    DB.getBy (UniqueAcctEmailPMPendingAcctAcctUuid $ tsUuidFor aUuid')
  return $ case pending of
    -- Hmm, it should have been pending if we just accepted it...
    Nothing -> []
    -- NB: can't yet remove the pending account, because we haven't successfully
    -- got an email address association with which to insert it into the main
    -- table
    Just (AcctEmailPMPendingAcct _ e) ->
      [BindEmailToAccountCommand e $ tsUuidFor aUuid']
trackAccounts aUuid' _ =
  -- NB: Account deletion should result in removal of all the email address
  -- associations as individual events, so we already handle that elsewhere.
  return []

trackAssocs
  :: (MonadIO m) => UuidFor EmailAddressEvent -> EmailAddressEvent
  -> ReaderT DB.SqlBackend m [Command]
trackAssocs
  eUuid' (EmailBindingToAccountRequestedEmailAddressEvent e aUuid') =
    DB.insertBy
      (AcctEmailPMPendingAssoc e (tsUuidFor eUuid') aUuid') >>=
      return . either
        (const [RejectDuplicateEmailBindingRequestCommand $ tsUuidFor eUuid'])
        (const [AcceptEmailBindingRequestCommand $ tsUuidFor eUuid'])
trackAssocs eUuid' EmailBindingRequestRejectedDuplicateEmailAddressEvent = do
  DB.deleteBy $ UniqueAcctEmailPMPendingAssocEmailUuid $ tsUuidFor eUuid'
  return []
trackAssocs eUuid' EmailBindingRequestAcceptedEmailAddressEvent = do
  pending <- fmap DB.entityVal <$>
    DB.getBy (UniqueAcctEmailPMPendingAssocEmailUuid $ tsUuidFor eUuid')
  case pending of
    -- Hmm, should have been pending if we accepted it
    Nothing -> return []
    Just (AcctEmailPMPendingAssoc e _ aUuid') -> do
      DB.insertBy $ AcctEmailPMAssoc e (tsUuidFor eUuid') aUuid'
      DB.deleteBy $ UniqueAcctEmailPMPendingAssocEmailUuid (tsUuidFor eUuid')
      -- This binding might have been triggered by the pending creation of a new
      -- account:
      DB.deleteBy $ UniqueAcctEmailPMPendingAcctEmailAddress e
      return []
trackAssocs eUuid' EmailRemovedEmailAddressEvent = do
  DB.deleteBy $ UniqueAcctEmailPMAssocEmailUuid $ tsUuidFor eUuid'
  return []

data AcctEmailPM
  = AcctEmailPM
  { aepmUpdate
      :: SequenceNumber -> UuidFor (TimeStamped Event) -> EventVersion
      -> TimeStamped Event -> IO [Command]
  , aepmEnsureAccountExistsForEmail
      :: EmailAddress -> EventT Event IO (UuidFor (TimeStamped AccountEvent))
  , aepmDeleteAccount :: ()
  , aepmBindEmailToAccount :: ()
  , aepmRemoveEmail :: ()
  }

instance IsView AcctEmailPM (TimeStamped Event) [Command] where
  viewName _ = "account_emails_process_manager"
  viewMigration _ = migrateAEPM
  viewUpdate = aepmUpdate

mkAccountEmailsProcessManager :: Pool DB.SqlBackend -> AcctEmailPM
mkAccountEmailsProcessManager pool =
    AcctEmailPM
      update
      ensureAccountExistsForEmailAddr
      deleteAccount
      bindEmailToAccount
      removeEmail
  where
    update _sn _uuid' _ev event =
      let _t = fst event in
      decomposeEvent
        noop
        noop
        noop
        noop
        (snd event)
    noop = const $ return []
    ensureAccountExistsForEmailAddr e = undefined
    deleteAccount = undefined
    bindEmailToAccount = undefined
    removeEmail = undefined
