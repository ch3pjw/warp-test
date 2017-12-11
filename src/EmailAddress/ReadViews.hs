{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module EmailAddress.ReadViews where

import Control.Monad (void)
import qualified  Database.Persist.Postgresql as DB
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import Events
  ( EmailAddress, TimeStamped, Event, AccountEvent
  , SessionEvent(SessionRequestedSessionEvent)
  , EmailAddressEvent(..), Command(..), decomposeEvent)
import ReadView (ProcessManager, simpleProcessManager, SimpleRvUpdate)
import UuidFor (UuidFor, coerceUuidFor)


share [mkPersist sqlSettings, mkMigrate "migrateAEPM"] [persistLowerCase|
AcctEmailPMEmailAcctAssoc
    emailAddress EmailAddress
    emailUuid (UuidFor (TimeStamped EmailAddressEvent))
    accountUuid (UuidFor (TimeStamped AccountEvent))
    UniqueAcctEmailPMEmailAcctAssocEmailUuid emailUuid
    UniqueAcctEmailPMEmailAcctAssocEmailAddr emailAddress
    deriving Show
|]


someUpdateThing :: SimpleRvUpdate SessionEvent [Command]
someUpdateThing sUuid' (SessionRequestedSessionEvent e) = do
    mAssoc <- DB.getBy $ UniqueAcctEmailPMEmailAcctAssocEmailAddr e
    case mAssoc of
      Just entity -> return
        -- FIXME: make special coerce for TimeStamped conversions:
        [ BindSessionToAccountCommand (coerceUuidFor sUuid')
        $ acctEmailPMEmailAcctAssocAccountUuid $ DB.entityVal entity ]
      Nothing -> undefined
someUpdateThing _sUuid' _ = return []

trackAssocs :: SimpleRvUpdate EmailAddressEvent [Command]
trackAssocs eaUuid' (EmailBoundToAccountEmailAddressEvent e aUuid') = do
  -- FIXME: this should probably at least log if we stumble across an
  -- association that already exists, even though that _should_ never happen.
  void $ DB.insertBy $
    AcctEmailPMEmailAcctAssoc e (coerceUuidFor eaUuid') aUuid'
  return []
trackAssocs eaUuid' EmailRemovedEmailAddressEvent = do
  DB.deleteBy $ UniqueAcctEmailPMEmailAcctAssocEmailUuid $ coerceUuidFor eaUuid'
  return []

accountEmailsProcessManager :: ProcessManager (TimeStamped Event) [Command]
accountEmailsProcessManager =
    simpleProcessManager "account_emails_process_manager" migrateAEPM
    update poll
  where
    update _uuid' event =
      let _t = fst event in
      decomposeEvent
        noop
        noop
        noop
        noop
        (snd event)
    noop = const $ return []
    poll = undefined