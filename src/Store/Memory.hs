module Store.Memory (newInMemoryStore) where

import Control.Monad (void)
import Control.Concurrent.STM (atomically)

import Eventful (
  getLatestStreamProjection, versionedStreamProjection)
import Eventful.Store.Memory (
  eventMapTVar, tvarEventStoreWriter, tvarEventStoreReader,
  ExpectedPosition(..), storeEvents)

import Events (unUuidFor)
import Store.Types (Store, newStoreFrom)

newInMemoryStore :: IO (Store event)
newInMemoryStore = do
    tvar <- eventMapTVar
    let
      w = tvarEventStoreWriter tvar
      r = tvarEventStoreReader tvar
    -- FIXME: AnyPosition always valid?
    newStoreFrom
      (\uuid' -> void . atomically . storeEvents w (unUuidFor uuid') AnyPosition)
      (\initialProjection uuid' -> atomically $ getLatestStreamProjection r $
          versionedStreamProjection (unUuidFor uuid') initialProjection)
