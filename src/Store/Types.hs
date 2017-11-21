{-# LANGUAGE Rank2Types #-}

module Store.Types where

import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID (UUID)
import Eventful (
    Projection, EventVersion,
    EventStoreReader(..), EventStoreWriter(..), EventWriteError,
    storeAndPublishEvents)
import Eventful.Store.Class (StreamEvent)

import Events (EventT, runEventT)

data Store m event = Store
  { sGetNotificationChan :: IO (U.OutChan (Maybe UUID))
  -- FIXME: sendShutdown feels weird, because it doesn't mean anything to
  -- actually writing to the store...
  , sSendShutdown :: IO ()
  , sRunEventT
      :: forall state a. (MonadIO m)
      => Projection state event -> EventT event state m a -> m a
  }

newStoreFrom
  :: (MonadIO m)
  => EventStoreWriter UUID EventVersion m event
  -> EventStoreReader UUID EventVersion m (StreamEvent UUID EventVersion event)
  -> IO (Store m event)
newStoreFrom writer reader = do
    -- We assume that the unused OutChan gets cleaned up when it goes out of
    -- scope:
    (i, _) <- U.newChan
    return $ Store
        (U.dupChan i)
        (U.writeChan i Nothing)
        (_runEventT i)
  where
    _runEventT i projection elt = do
        runEventT elt projection reader $ storeAndPublishEvents
          writer [\uuid event -> liftIO $ U.writeChan i $ Just uuid]

liftEventStoreWriter
  :: (m (Maybe (EventWriteError pos)) -> n (Maybe (EventWriteError pos)))
  -> EventStoreWriter key pos m event
  -> EventStoreWriter key pos n event
liftEventStoreWriter f (EventStoreWriter w) =
    EventStoreWriter $ \k p es -> f $ w k p es

liftEventStoreReader
  :: (m [event] -> n [event])
  -> EventStoreReader key pos m event
  -> EventStoreReader key pos n event
liftEventStoreReader f (EventStoreReader r) = EventStoreReader $ f . r
