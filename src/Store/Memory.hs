module Store.Memory (newInMemoryStore) where

import Control.Concurrent.STM (atomically)

import Eventful.Store.Memory (
  eventMapTVar, tvarEventStoreWriter, tvarEventStoreReader)

import Store.Types
  (Store
  , newStoreFrom
  , liftEventStoreReader
  , liftEventStoreWriter)

newInMemoryStore :: IO (Store IO event)
newInMemoryStore = do
    tvar <- eventMapTVar
    let
      w = liftEventStoreWriter atomically $ tvarEventStoreWriter tvar
      r = liftEventStoreReader atomically $ tvarEventStoreReader tvar
    newStoreFrom w r
