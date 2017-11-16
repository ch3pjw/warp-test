{-# LANGUAGE Rank2Types #-}

module Store.Types where

import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Writer (runWriterT)
import Data.UUID (UUID)
import Eventful (
    Projection, StreamProjection, EventVersion, streamProjectionState)

import Events (EventT, UuidFor)

data Store event = Store
  { sGetNotificationChan :: IO (U.OutChan (Maybe (UuidFor event)))
  -- FIXME: sendShutdown feels weird, because it doesn't mean anything to
  -- actually writing to the store...
  , sSendShutdown :: IO ()
  , sRunEventT :: forall m a. (MonadIO m) => EventT event m a -> UuidFor event -> m a
  , sRunEventTWithState :: forall state m a. (MonadIO m) => Projection state event -> (state -> EventT event m a) -> UuidFor event -> m a
  }

newStoreFrom
  :: (UuidFor event -> [event] -> IO ())
  -> (forall state. Projection state event -> UuidFor event
       -> IO (StreamProjection UUID EventVersion state event))
  -> IO (Store event)
newStoreFrom write getLatestProjection = do
    -- We assume that the unused OutChan gets cleaned up when it goes out of
    -- scope:
    (i, _) <- U.newChan
    return $ Store
        (U.dupChan i)
        (U.writeChan i Nothing)
        (_runEventT i)
        (_runEventTWithState i)
  where
    getLatestState initialProjection uuid' =
        streamProjectionState <$> getLatestProjection initialProjection uuid'
    _runEventT i eventT uuid' = do
        (result, events) <- runWriterT eventT
        liftIO $ write uuid' events
        when (not . null $ events) $ liftIO $ U.writeChan i (Just uuid')
        return result
    _runEventTWithState i initialProjection f uuid' = do
        state <- liftIO $ getLatestState initialProjection uuid'
        _runEventT i (f state) uuid'
