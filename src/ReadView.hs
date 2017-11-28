{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadView
  ( DB.EntityField(..)
  , ViewSequenceNumberId
  , ReadView(..)
  , readView, simpleReadView
  , viewWorker
  , runReadViews
  , liftReadView
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

import Events (UuidFor(..), coerceUuidFor)
import Store (eventStoreConfig, untilNothing)


share [mkPersist sqlSettings, mkMigrate "migrateVSN"] [persistLowerCase|
ViewSequenceNumber
    name Text
    latestApplied SequenceNumber
    UniqueName name
    deriving Show
|]

type RvUpdate event returnType
    = SequenceNumber -> UuidFor event -> EventVersion -> event
    -> ReaderT DB.SqlBackend IO returnType -- FIXME: <- is a terrible name

type SimpleRvUpdate event returnType
    = UuidFor event -> event -> ReaderT DB.SqlBackend IO returnType -- <- still bad

data ReadView event = ReadView
  { rvTableName :: Text
  , rvMigration :: DB.Migration
  , rvUpdate :: RvUpdate event ()
  }

instance Show (ReadView event) where
    show rv = unpack $ "<ReadView " <> (rvTableName rv) <> ">"

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
  :: ReadView event -> [GlobalStreamEvent event] -> ReaderT DB.SqlBackend IO ()
handleReadViewEvents rv events = do
    mapM_ (applyUpdate . decomposeEvent) events
    updateSN events
  where
    decomposeEvent e = let e' = streamEventEvent e in
        ( streamEventPosition e
        , UuidFor $ streamEventKey e'
        , streamEventPosition e'
        , streamEventEvent e')
    applyUpdate (globalPos, streamKey, streamPos, streamEventData) =
        rvUpdate rv globalPos streamKey streamPos streamEventData
    updateSN [] = return ()
    updateSN es = let latestSN = streamEventPosition $ last es in
        DB.updateWhere
          [ViewSequenceNumberName ==. rvTableName rv]
          [ViewSequenceNumberLatestApplied =. latestSN]


initialiseReadView :: ReadView event -> ReaderT DB.SqlBackend IO ()
initialiseReadView rv = do
    DB.runMigration $ rvMigration rv
    void . getOrInitSequenceNumber $ rvTableName rv

getOrInitSequenceNumber :: Text -> ReaderT DB.SqlBackend IO SequenceNumber
getOrInitSequenceNumber tableName = do
    eVsn <- DB.insertBy $ ViewSequenceNumber tableName 0
    return $ either (getN . DB.entityVal) (const 0) eVsn
  where
    getN (ViewSequenceNumber _ n) = n


updateReadView
  :: (ToJSON event, FromJSON event)
  => ReadView event -> ReaderT DB.SqlBackend IO ()
updateReadView rv =
      getOrInitSequenceNumber (rvTableName rv) >>=
      latestEvents >>= handleReadViewEvents rv


viewWorker
  :: (ToJSON event, FromJSON event)
  => ReadView event
  -> Pool DB.SqlBackend
  -> (Maybe () -> IO ())
  -> (IO (Maybe a))
  -> IO ()
viewWorker rv pool notify wait = DB.runSqlPool i pool >> f >> notify Nothing
  where
    f = untilNothing wait (const $ DB.runSqlPool updateAndNotify pool)
    i = initialiseReadView rv >> updateAndNotify
    updateAndNotify = updateReadView rv >> liftIO (notify $ Just ())


runReadViews
  :: (ToJSON event, FromJSON event)
  => [(Maybe () -> IO (), ReadView event)]
  -> Pool DB.SqlBackend -> (IO (IO (Maybe a))) -> IO ()
runReadViews notificationActionsAndRvs pool getWait = do
    -- Taking the pairs of ReadView and corresponding notification channel feels
    -- clunky - but the alternative, as we have in Store, where we construct a
    -- queue internally in an IO action that returns the data structure doesn't
    -- feel great either...
    DB.runSqlPool (DB.runMigration migrateVSN) pool
    as <- mapM (uncurry runViewWorker) notificationActionsAndRvs
    mapM_ A.link as
    mapM_ A.wait as
  where
    runViewWorker n rv = A.async $ getWait >>= viewWorker rv pool n

liftReadView :: (event' -> Maybe event) -> ReadView event -> ReadView event'
liftReadView f rv = rv { rvUpdate = rvUpdate' }
  where
    rvUpdate' sn uuid version event' =
      maybe (return ()) (rvUpdate rv sn (coerceUuidFor uuid) version) (f event')
