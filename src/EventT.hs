module EventT
  ( EventT, runEventT, logEvents, logEvents_, getState
  , mapEvents, liftToEvent)
  where

import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..), withReaderT, ask)
import Data.Maybe (mapMaybe)
import Data.UUID (UUID)

import Eventful (
    ExpectedPosition, EventStoreReader(..), EventWriteError, StreamEvent,
    getLatestStreamProjection, versionedStreamProjection,
    streamProjectionState, EventVersion, Projection)

import Events (Event, ToEvent, toEvent)


type StoreEvents key event m =
    key -> ExpectedPosition EventVersion -> [event]
    -> m (Maybe (EventWriteError EventVersion))

type EventT event m
  = ReaderT
  ( EventStoreReader UUID EventVersion m (StreamEvent UUID EventVersion event)
  , StoreEvents UUID event m
  ) m

runEventT
    :: (Monad m)
    => EventT event m a
    -> EventStoreReader UUID EventVersion m (StreamEvent UUID EventVersion event)
    -> StoreEvents UUID event m -> m a
runEventT et esr se = runReaderT et (esr, se)

logEvents
    :: (Monad m)
    => UUID -> ExpectedPosition EventVersion -> [event]
    -> EventT event m (Maybe (EventWriteError EventVersion))
logEvents uuid pos events = ask >>= (\(_, se) -> lift $ se uuid pos events)

logEvents_
    :: (Monad m)
    => UUID -> ExpectedPosition EventVersion -> [event]
    -> EventT event m ()
logEvents_ uuid pos = void . logEvents uuid pos

getState
    :: (Monad m)
    => Projection state event -> UUID -> EventT event m state
getState proj key = ask >>= (\(reader, _) -> lift $
    streamProjectionState <$>
    getLatestStreamProjection reader (versionedStreamProjection key proj))

mapEvents
    :: (Monad m) => (event -> event') -> (event' -> Maybe event)
    -> EventT event m a
    -> EventT event' m a
mapEvents f f' et = withReaderT convert et
  where
    convert (reader, storeEvents) =
      (overReader reader, \k p -> storeEvents k p . fmap f)
    -- reader is a functor that internally deals in a list of events. We want to
    -- be able to drop events from that list if they're not pertinent to
    -- reconsituting our state, so we define the following:
    overReader (EventStoreReader reader) =
        EventStoreReader $ fmap (mapMaybe (sequence . fmap f')) <$> reader

liftToEvent
    :: (Monad m, ToEvent event)
    => (Event -> Maybe event)
    -> EventT event m a
    -> EventT Event m a
liftToEvent f' = mapEvents toEvent f'
