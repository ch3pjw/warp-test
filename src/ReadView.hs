{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module ReadView
  ( DB.EntityField(..)
  , ViewSequenceNumberId
  , IsView(..)
  , ReadView(..), RvUpdate
  , readView, simpleReadView, SimpleRvUpdate, toRvUpdate
  , setupForReadViews
  , viewWorker
  , liftReadView
  , ProcessManager(..), simpleProcessManager
  ) where

import qualified Control.Concurrent.Async as A
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Data.Aeson (ToJSON, FromJSON)
import Data.Monoid
import Data.Pool (Pool)
import Data.Text (Text, unpack)
import Database.Persist.Postgresql ((=.), (==.))
import qualified Database.Persist.Postgresql as DB
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Eventful (
    SequenceNumber(..), EventVersion, GlobalStreamEvent,
    getEvents, eventsStartingAt, streamEventEvent, streamEventKey,
    streamEventPosition)
import Eventful.Store.Postgresql (serializedGlobalEventStoreReader)
import Eventful.Store.Sql (
    jsonStringSerializer,
    sqlGlobalEventStoreReader)

import Store (eventStoreConfig, untilNothing)
import UuidFor (UuidFor(..), coerceUuidFor)


share [mkPersist sqlSettings, mkMigrate "migrateVSN"] [persistLowerCase|
ViewSequenceNumber
    name Text
    latestApplied SequenceNumber
    UniqueName name
    deriving Show
|]

-- FIXME: name!!!
type RvUpdate event returnType
    = SequenceNumber -> UuidFor event -> EventVersion -> event
    -> IO returnType -- FIXME: <- is a terrible name

type SimpleRvUpdate event returnType
    = UuidFor event -> event -> IO returnType -- <- still bad

toRvUpdate :: SimpleRvUpdate event x -> RvUpdate event x
toRvUpdate f _ uuid' _ e = f uuid' e


class IsView a event x | a -> event, a -> x where
  viewName :: a -> Text
  viewMigration :: a -> DB.Migration
  viewUpdate :: a -> RvUpdate event x
data ReadView event = ReadView
  { rvTableName :: Text
  , rvMigration :: DB.Migration
  , rvUpdate :: RvUpdate event ()
  }

instance Show (ReadView event) where
    show rv = unpack $ "<ReadView " <> (rvTableName rv) <> ">"

instance IsView (ReadView event) event () where
  viewName = rvTableName
  viewMigration = rvMigration
  viewUpdate = rvUpdate

readView :: Text -> DB.Migration -> RvUpdate event () -> ReadView event
readView = ReadView

-- | A simple read view doesn't give you access to event version information
--   from the event log, just events and the keys of the events streams for
--   those events.
simpleReadView
  :: Text -> DB.Migration
  -> SimpleRvUpdate event ()
  -> ReadView event
simpleReadView tableName migration update =
    readView tableName migration update'
  where
    update' _ uuid _ event = update uuid event


latestEvents
  :: (MonadIO m, ToJSON event, FromJSON event)
  => SequenceNumber
  -> DB.SqlPersistT m [GlobalStreamEvent event]
latestEvents latestHandled =
    getEvents eventReader (eventsStartingAt () $ latestHandled + 1)
  where
    jsonReader = sqlGlobalEventStoreReader eventStoreConfig
    eventReader = serializedGlobalEventStoreReader jsonStringSerializer jsonReader

handleReadViewEvents
  :: (IsView a event x, Monoid x)
  => a -> [GlobalStreamEvent event]
  -> ReaderT DB.SqlBackend IO x
handleReadViewEvents v events = do
    results <- liftIO $ mapM (applyUpdate . decomposeEvent) events
    updateSN events
    return $ mconcat results
  where
    -- FIXME: decomposeEvent is already defined in Events.hs
    decomposeEvent e = let e' = streamEventEvent e in
        ( streamEventPosition e
        , UuidFor $ streamEventKey e'
        , streamEventPosition e'
        , streamEventEvent e')
    applyUpdate (globalPos, streamKey, streamPos, streamEventData) =
        viewUpdate v globalPos streamKey streamPos streamEventData
    updateSN [] = return ()
    updateSN es = let latestSN = streamEventPosition $ last es in
        DB.updateWhere
          [ViewSequenceNumberName ==. viewName v]
          [ViewSequenceNumberLatestApplied =. latestSN]


initialiseReadView
  :: (IsView a event x) => a -> ReaderT DB.SqlBackend IO ()
initialiseReadView v = do
    DB.runMigration $ viewMigration v
    void . getOrInitSequenceNumber $ viewName v

getOrInitSequenceNumber :: Text -> ReaderT DB.SqlBackend IO SequenceNumber
getOrInitSequenceNumber tableName = do
    eVsn <- DB.insertBy $ ViewSequenceNumber tableName 0
    return $ either (getN . DB.entityVal) (const 0) eVsn
  where
    getN (ViewSequenceNumber _ n) = n


updateReadView
  :: (ToJSON event, FromJSON event, IsView a event x, Monoid x)
  => a -> ReaderT DB.SqlBackend IO x
updateReadView v =
      getOrInitSequenceNumber (viewName v) >>=
      latestEvents >>= handleReadViewEvents v


viewWorker
  :: (ToJSON event, FromJSON event, IsView v event x, Monoid x)
  => v
  -> Pool DB.SqlBackend
  -> (Maybe x -> IO ())
  -> (IO (Maybe a))
  -> IO ()
viewWorker v pool notify wait = DB.runSqlPool i pool >> f >> notify Nothing
  where
    f = untilNothing wait (const $ DB.runSqlPool updateAndNotify pool)
    i = initialiseReadView v >> updateAndNotify
    updateAndNotify = updateReadView v >>= liftIO . notify . Just


setupForReadViews :: Pool DB.SqlBackend -> IO ()
setupForReadViews = DB.runSqlPool (DB.runMigration migrateVSN)

liftReadView :: (event' -> Maybe event) -> ReadView event -> ReadView event'
liftReadView f rv = rv { rvUpdate = rvUpdate' }
  where
    rvUpdate' sn uuid version event' =
      maybe (return ()) (rvUpdate rv sn (coerceUuidFor uuid) version) (f event')


data ProcessManager event command = ProcessManager
  { pmName :: Text
  , pmMigration :: DB.Migration
  , pmUpdate :: RvUpdate event [command]
  , pmPoll :: ReaderT DB.SqlBackend IO [command]
  }

simpleProcessManager
  :: Text -> DB.Migration
  -> SimpleRvUpdate event [command]
  -> ReaderT DB.SqlBackend IO [command]
  -> ProcessManager event command
simpleProcessManager name migration update poll =
    ProcessManager name migration update' poll
  where
    update' _ uuid _ event = update uuid event

instance IsView (ProcessManager event command) event [command] where
  viewName = pmName
  viewMigration = pmMigration
  viewUpdate = pmUpdate
