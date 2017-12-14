{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Accounts.ReadViews where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.List as List
import Data.Text (Text)
import qualified  Database.Persist.Postgresql as DB
import Database.Persist.Postgresql ((=.), (==.))
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import Eventful (EventVersion)

import Events (TimeStamped, Event, AccountEvent(..), decomposeEvent, tsUuidFor)
import ReadView (SimpleRvUpdate, ReadView, simpleReadView)
import UuidFor (UuidFor, coerceUuidFor)

share [mkPersist sqlSettings, mkMigrate "migrateAVA"] [persistLowerCase|
AcctViewAccount
    uuid (UuidFor (TimeStamped AccountEvent))
    version EventVersion
    name Text
    serviceEmailsEnabled Bool
    UniqueAcctViewAcctUuid uuid
    deriving Show
|]


-- updateAccountRow
--   :: UuidFor AccountEvent -> EventVersion -> [DB.Update AcctViewAccount]
--   -> ReaderT DB.SqlBackend IO ()
-- updateAccountRow aUuid' version updates =
--   DB.updateWhere
--     [AcctViewAccountUuid ==. tsUuidFor aUuid']
--     ((AcctViewAccountVersion =. version) : updates)


-- updateAccount :: RvUpdate AccountEvent ()
-- updateAccount _ aUuid' version AccountCreatedAccountEvent =
--   void $ DB.insertBy $ AcctViewAccount (tsUuidFor aUuid') version "" True
-- updateAccount _ aUuid' version (AccountNameUpdatedAccountEvent name) =
--   updateAccountRow aUuid' version [AcctViewAccountName =. name]
-- updateAccount _ aUuid' version ServiceUpdateEmailsEnabledAccountEvent =
--   updateAccountRow aUuid' version [AcctViewAccountServiceEmailsEnabled =. True]
-- updateAccount _ aUuid' version ServiceUpdateEmailsDisabledAccountEvent =
--   updateAccountRow aUuid' version [AcctViewAccountServiceEmailsEnabled =. False]
-- updateAccount _ aUuid' _ AccountDeletedAccountEvent =
--   DB.deleteBy $ UniqueAcctViewAcctUuid $ tsUuidFor aUuid'

-- type Waiters = [((UuidFor AccountEvent, EventVersion), IO ())]

-- popWaiters
--   :: UuidFor AccountEvent -> EventVersion -> Waiters -> (Waiters, [IO ()])
-- popWaiters aUuid' version waiters =
--     fmap snd <$> List.partition (predicate . fst) waiters
--   where
--     predicate (reqAUuid', reqVersion) =
--         reqAUuid' == aUuid' && reqVersion < version


-- toMonoid :: (Monoid x) => Maybe x -> x
-- toMonoid Nothing = mempty
-- toMonoid (Just x) = x

-- newAccountReadView :: IO (WaitableReadView (TimeStamped Event))
-- newAccountReadView = do
--     waitersV <- newMVar (mempty :: Waiters)
--     return $
--       WaitableReadView "accounts_read_view" migrateAVA
--       (update waitersV)
--       (waitNewer waitersV)
--   where
--     update waitersV sn uuid' version event =
--       let aUuid' = coerceUuidFor uuid' in do
--         liftIO $ modifyMVar waitersV $ \waiters ->
--           sequence $ mapM_ id <$> popWaiters aUuid' version waiters
--         decomposeEvent
--           noop
--           noop
--           (updateAccount sn aUuid' version)
--           noop
--           (snd event)
--     noop = const $ return ()
--     waitNewer waitersV uuid' version = do
--       mEntity <- DB.getBy $ UniqueAcctViewAcctUuid (coerceUuidFor uuid')
--       let curVers = maybe (-1) (acctViewAccountVersion . DB.entityVal) mEntity
--       if curVers > version
--         then return ()
--         else do
--             v <- liftIO $ newEmptyMVar
--             liftIO $ modifyMVar_ waitersV $ \waiters -> return $
--                 ((coerceUuidFor uuid', version), putMVar v ()) : waiters
--             liftIO $ takeMVar v
