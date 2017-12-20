module EventT
  ( EventT, runEventT
  , logEvents, logEvents_
  , getStreamProjection
  , getState
  , logNewStream
  , logWithLatest, logWithLatest_
  , mapEvents, liftToEvent, timeStamp
  , logEvents', logEvents_'
  , getStreamProjection'
  , getState'
  , logNewStream'
  , logWithLatest', logWithLatest_'
  )
  where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT(..), withReaderT, ask)
import Data.Maybe (mapMaybe)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUIDv4

import Eventful
  ( ExpectedPosition(..), EventStoreReader(..), EventWriteError
  , StreamEvent, StreamProjection(..), getLatestStreamProjection
  , versionedStreamProjection, EventVersion, Projection)

import Events (Event, ToEvent, toEvent, TimeStamped)
import UuidFor (UuidFor(..))


type StoreEvents key event m =
    key -> ExpectedPosition EventVersion -> [event]
    -> m (Either (EventWriteError EventVersion) EventVersion)

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

-- FIXME: this should probably translate the kinda unhelpful "maybe error" into
-- something more canonical, like "Either String ()"
logEvents
    :: (Monad m)
    => UUID -> ExpectedPosition EventVersion -> [event]
    -> EventT event m (Either (EventWriteError EventVersion) EventVersion)
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

logNewStream :: (MonadIO m) => [event] -> EventT event m UUID
logNewStream events = untilRight go
  where
    go = do
      uuid <- liftIO UUIDv4.nextRandom
      fmap (const uuid) <$> logEvents uuid NoStream events
    untilRight :: (Monad m) => m (Either a b) -> m b
    untilRight m = m >>= either (const $ untilRight m) return

-- | Use the given initial projection to project the latest state from the
-- store, use the given (side-effect free) function to generate new events,
-- which are then committed to the store only if nothing has changed the
-- projection in the intervening time.
logWithLatest
    :: (Monad m)
    => Projection state event -> UUID -> (state -> ([event], a))
    -> EventT event m (EventVersion, a)
logWithLatest proj uuid f = do
    streamProj <- getStreamProjection proj uuid
    let (events, result) = f $ streamProjectionState streamProj
    eError <- logEvents uuid
        (ExactPosition $ streamProjectionPosition streamProj)
        events
    either retry (\ev -> return (ev, result)) eError
  where
    retry _ = logWithLatest proj uuid f

logWithLatest_
  :: (Monad m)
  => Projection state event -> UUID -> (state -> [event])
  -> EventT event m EventVersion
logWithLatest_ proj uuid f =
    fmap fst $ logWithLatest proj uuid $ flip (,) () . f

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

timeStamp
  :: (MonadIO m)
  => IO UTCTime -> EventT event m a
  -> EventT (TimeStamped event) m a
timeStamp getT eventT = withReaderT convert eventT
  where
    convert (esr, storeEvents) =
      ( fmap (fmap snd) esr
      , \k p es -> liftIO (tagTime es) >>= storeEvents k p)

    tagTime :: [event] -> IO [TimeStamped event]
    tagTime = traverse (\e -> flip (,) e <$> getT)


logEvents'
    :: (Monad m)
    => UuidFor event -> ExpectedPosition EventVersion -> [event]
    -> EventT event m (Either (EventWriteError EventVersion) EventVersion)
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

logNewStream' :: (MonadIO m) => [event] -> EventT event m (UuidFor event)
logNewStream' = fmap UuidFor . logNewStream

logWithLatest'
    :: (Monad m)
    => Projection state event -> UuidFor event -> (state -> ([event], a))
    -> EventT event m (EventVersion, a)
logWithLatest' proj uuid' = logWithLatest proj $ unUuidFor uuid'

logWithLatest_'
    :: (Monad m)
    => Projection state event -> UuidFor event -> (state -> [event])
    -> EventT event m EventVersion
logWithLatest_' proj uuid' = logWithLatest_ proj $ unUuidFor uuid'
