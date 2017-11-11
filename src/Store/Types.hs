{-# LANGUAGE Rank2Types #-}

module Store.Types where

import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad (when)
import Data.UUID (UUID)
import Eventful (
    Projection, StreamProjection, EventVersion, streamProjectionState)

-- | An Action is a side-effect that runs on a particular stream's state and
-- | reports what it did as events
type Action state event = state -> IO [event]

data Store event = Store
  { sGetNotificationChan :: IO (U.OutChan (Maybe UUID))
  -- FIXME: sendShutdown feels weird, because it doesn't mean anything to
  -- actually writing to the store...
  , sSendShutdown :: IO ()
  , _sUpdate :: forall state. Projection state event -> Action state event -> UUID -> IO ()
  -- FIXME: probably for testing only?
  , sPoll :: forall state. Projection state event -> UUID -> IO state
  }

updateStore
  :: Projection state event -> Action state event -> Store event -> UUID
  -> IO ()
updateStore p a s u = _sUpdate s p a u

newStoreFrom
  :: (UUID -> [event] -> IO ())
  -> (forall state. Projection state event -> UUID
       -> IO (StreamProjection UUID EventVersion state event))
  -> IO (Store event)
newStoreFrom write getLatestProjection = do
    -- We assume that the unused OutChan gets cleaned up when it goes out of
    -- scope:
    (i, _) <- U.newChan
    let update initialProjection a uuid = do
            events <- getLatestState initialProjection uuid >>= a
            write uuid events
            when (not . null $ events) $ U.writeChan i (Just uuid)
        getLatestState initialProjection uuid =
            streamProjectionState <$> getLatestProjection initialProjection uuid
    return $
      Store (U.dupChan i) (U.writeChan i Nothing) update getLatestState
