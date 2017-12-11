{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Accounts.ReadViews where

import Control.Monad (void)
import Data.Text (Text)
import qualified  Database.Persist.Postgresql as DB
import Database.Persist.Postgresql ((=.), (==.))
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import Events (TimeStamped, Event, AccountEvent(..), decomposeEvent, tsUuidFor)
import ReadView (SimpleRvUpdate, ReadView, simpleReadView)
import UuidFor (UuidFor, coerceUuidFor)

share [mkPersist sqlSettings, mkMigrate "migrateAVA"] [persistLowerCase|
AcctViewAccount
    uuid (UuidFor (TimeStamped AccountEvent))
    name Text
    serviceEmailsEnabled Bool
    UniqueAcctViewAcctUuid uuid
    deriving Show
|]


updateAccount :: SimpleRvUpdate AccountEvent ()
updateAccount aUuid' AccountCreatedAccountEvent =
  void $ DB.insertBy $ AcctViewAccount (tsUuidFor aUuid') "" True
updateAccount aUuid' (AccountNameUpdatedAccountEvent name) =
  DB.updateWhere
    [AcctViewAccountUuid ==. tsUuidFor aUuid']
    [AcctViewAccountName =. name]
updateAccount aUuid' ServiceUpdateEmailsEnabledAccountEvent =
  DB.updateWhere
    [AcctViewAccountUuid ==. tsUuidFor aUuid']
    [AcctViewAccountServiceEmailsEnabled =. True]
updateAccount aUuid' ServiceUpdateEmailsDisabledAccountEvent =
  DB.updateWhere
    [AcctViewAccountUuid ==. tsUuidFor aUuid']
    [AcctViewAccountServiceEmailsEnabled =. False]
updateAccount aUuid' AccountDeletedAccountEvent =
  DB.deleteBy $ UniqueAcctViewAcctUuid $ tsUuidFor aUuid'

accountEmailsProcessManager :: ReadView (TimeStamped Event)
accountEmailsProcessManager =
    simpleReadView "accounts_read_view" migrateAVA
    update
  where
    update uuid' event =
      decomposeEvent
        noop
        noop
        (updateAccount (coerceUuidFor uuid'))
        noop
        (snd event)
    noop = const $ return ()
