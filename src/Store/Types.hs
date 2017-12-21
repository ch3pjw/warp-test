{-# LANGUAGE Rank2Types #-}

module Store.Types
  ( Store(..), newStoreFrom, liftEventStoreWriter, liftEventStoreReader
  , untilNothing, reactivelyRunEventT
  ) where

import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID (UUID)
import Eventful (
    EventVersion,
    EventStoreReader(..), EventStoreWriter(..), EventWriteError,
    storeAndPublishEvents)
import Eventful.Store.Class (StreamEvent)

import EventT (EventT, runEventT)
import UuidFor (UuidFor(..))

data Store m event = Store
  { sGetWaitUpdate :: IO (IO (Maybe (UuidFor event)))
  , sWaitFor :: UuidFor event -> (event -> Bool) -> IO (Maybe event)
  -- FIXME: sendShutdown feels weird, because it doesn't mean anything to
  -- actually writing to the store...
  , sSendShutdown :: IO ()
  , sRunEventT :: forall a. (MonadIO m) => EventT event m a -> m a
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
        (getWaitUpdate i)
        (waitFor i)
        (U.writeChan i Nothing)
        (_runEventT i)
  where
    _runEventT i elt =
        runEventT elt reader $ storeAndPublishEvents writer
          [\uuid event -> liftIO $ U.writeChan i $ Just (UuidFor uuid, event)]
    getWaitUpdate i = do
      o <- U.dupChan i
      return $ fmap fst <$> U.readChan o
    waitFor i requestedUuid' p = U.dupChan i >>= loop
      where
        loop o = do
          a <- U.readChan o
          case a of
            Just (uuid', event) ->
              if uuid' == requestedUuid' && p event
                then return (Just event)
                else loop o
            Nothing -> return Nothing


liftEventStoreWriter
  :: (  m (Either (EventWriteError pos) EventVersion)
     -> n (Either (EventWriteError pos) EventVersion))
  -> EventStoreWriter key pos m event
  -> EventStoreWriter key pos n event
liftEventStoreWriter f (EventStoreWriter w) =
    EventStoreWriter $ \k p es -> f $ w k p es

liftEventStoreReader
  :: (m [event] -> n [event])
  -> EventStoreReader key pos m event
  -> EventStoreReader key pos n event
liftEventStoreReader f (EventStoreReader r) = EventStoreReader $ f . r


untilNothing :: (MonadIO m) => IO (Maybe a) -> (a -> m ()) -> m ()
untilNothing wait f =
    liftIO wait >>=
    maybe (return ()) (\a -> f a >> untilNothing wait f)

reactivelyRunEventT
  :: (MonadIO m)
  => (x -> EventT event m ())
  -> IO (Maybe x) -> Store m event -> m ()
reactivelyRunEventT f waitX store =
    untilNothing waitX $ \x -> sRunEventT store (f x)
