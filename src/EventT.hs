module EventT
  ( EventT, runEventT
  , logEvents, logEvents_
  , getStreamProjection
  , getState
  , logWithLatest
  , mapEvents, liftToEvent
  , logEvents', logEvents_'
  , getStreamProjection'
  , getState'
  , logWithLatest'
  )
  where

import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..), withReaderT, ask)
import Data.Maybe (mapMaybe)
import Data.UUID (UUID)

import Eventful
  ( ExpectedPosition(ExactPosition), EventStoreReader(..), EventWriteError
  , StreamEvent, StreamProjection(..), getLatestStreamProjection
  , versionedStreamProjection, EventVersion, Projection)

import Events (Event, ToEvent, toEvent, TimeStamped)
import UuidFor (UuidFor(..))


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

-- | Given an initial stream projection, retrieve the events for the stream
-- identified by the given UUID apply them, returning the latest projection.
getStreamProjection
    :: (Monad m)
    => Projection state event -> UUID
    -> EventT event m (StreamProjection UUID EventVersion state event)
getStreamProjection proj key = ask >>= (\(reader, _) -> lift $
     getLatestStreamProjection reader (versionedStreamProjection key proj))

getState
    :: (Monad m)
    => Projection state event -> UUID -> EventT event m state
getState proj key = streamProjectionState <$> getStreamProjection proj key

-- | Use the given initial projection to project the latest state from the
-- store, use the given (side-effect free) function to generate new events,
-- which are then committed to the store only if nothing has changed the
-- projection in the intervening time.
logWithLatest
    :: (Monad m)
    => Projection state event -> UUID -> (state -> ([event], a))
    -> EventT event m a
logWithLatest proj uuid f = do
    streamProj <- getStreamProjection proj uuid
    let (events, result) = f $ streamProjectionState streamProj
    mError <- logEvents uuid
        (ExactPosition $ streamProjectionPosition streamProj)
        events
    maybe (return result) retry mError
  where
    retry _ = logWithLatest proj uuid f

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


logEvents'
    :: (Monad m)
    => UuidFor event -> ExpectedPosition EventVersion -> [event]
    -> EventT event m (Maybe (EventWriteError EventVersion))
logEvents' uuid' = logEvents $ unUuidFor uuid'

logEvents_'
    :: (Monad m)
    => UuidFor event -> ExpectedPosition EventVersion -> [event]
    -> EventT event m ()
logEvents_' uuid' = logEvents_ $ unUuidFor uuid'

getStreamProjection'
    :: (Monad m)
    => Projection state event -> UuidFor event
    -> EventT event m (StreamProjection UUID EventVersion state event)
getStreamProjection' proj key = getStreamProjection proj $ unUuidFor key

getState'
    :: (Monad m)
    => Projection state event -> UuidFor event -> EventT event m state
getState' proj uuid' = getState proj $ unUuidFor uuid'

logWithLatest'
    :: (Monad m)
    => Projection state event -> UuidFor event -> (state -> ([event], a))
    -> EventT event m a
logWithLatest' proj uuid' = logWithLatest proj $ unUuidFor uuid'
