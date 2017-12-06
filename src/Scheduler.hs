{-# LANGUAGE DeriveFunctor #-}

module Scheduler where

import Prelude hiding (fail)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait, poll)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Exception (bracket)
import Control.Monad (filterM)
import Control.Monad.Fail (MonadFail, fail)
import Data.List (insertBy, partition)
import Data.Maybe (isNothing)
import Data.Time.Clock
  ( UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime
  , diffTimeToPicoseconds, picosecondsToDiffTime, secondsToDiffTime)


data TimeInterface
  = TimeInterface
  { getTime :: IO UTCTime
  , delay :: Int -> IO ()
  }

normalTimeInterface :: TimeInterface
normalTimeInterface = TimeInterface getCurrentTime threadDelay

nominalDiffTimeToMicroseconds :: NominalDiffTime -> Int
nominalDiffTimeToMicroseconds =
    (`div` 1000000) . fromInteger . diffTimeToPicoseconds . fromRational
    . toRational

microsecondsToNominalDiffTime :: Int -> NominalDiffTime
microsecondsToNominalDiffTime =
    (* 1000000) . fromRational . toRational . picosecondsToDiffTime . toInteger

mkTimeout :: Integer -> NominalDiffTime
mkTimeout = fromRational . toRational . secondsToDiffTime


data Scheduled a
  = Scheduled
  { schedAfter :: Maybe UTCTime
  , schedBefore :: Maybe UTCTime
  , schedAction :: a
  } deriving (Eq, Show, Functor)

actNow :: a -> Scheduled a
actNow a = Scheduled Nothing Nothing a

actAfter :: UTCTime -> a -> Scheduled a
actAfter t a = Scheduled (Just t) Nothing a

actBefore :: UTCTime -> a -> Scheduled a
actBefore t a = Scheduled Nothing (Just t) a

actBetween :: (MonadFail m) => UTCTime -> UTCTime -> a -> m (Scheduled a)
actBetween t1 t2 a
  | t1 < t2 = return $ Scheduled (Just t1) (Just t2) a
  | otherwise = fail "Can't create scheduled action with before < after"

data Delayed a
  = Delayed
  { dUntil :: UTCTime
  , dAction :: a
  } deriving (Eq, Show, Functor)

compareDelayedUntils :: Delayed a -> Delayed a -> Ordering
compareDelayedUntils d1 d2 = compare (dUntil d1) (dUntil d2)

newtype DelayedList a =
    DelayedList {unDelayedList :: [Delayed a]} deriving (Eq, Show, Functor)

instance Foldable DelayedList where
    foldr f acc = foldr (f . dAction) acc . unDelayedList
    length = length . unDelayedList

dlEmpty :: DelayedList a
dlEmpty = DelayedList []

dlSingleton :: Delayed a -> DelayedList a
dlSingleton = flip dlInsert dlEmpty

dlInsert :: Delayed a -> DelayedList a -> DelayedList a
dlInsert d = DelayedList . insertBy compareDelayedUntils d . unDelayedList

dlSplitAfter
  :: UTCTime -> DelayedList a -> (DelayedList a, DelayedList a)
dlSplitAfter untilT (DelayedList ds) = (DelayedList before, DelayedList after)
  where
    (before, after) = partition isBeforeOrAt ds
    isBeforeOrAt (Delayed u _) = u <= untilT


data QueuedStatus = Queued | TooLate deriving (Eq, Show)


data Scheduler
  = Scheduler
  { schedule :: Scheduled (IO ()) -> IO QueuedStatus
  , runScheduler :: IO ()
  , stopScheduler :: IO ()
  }

data QueueAction = Tick | Run (Delayed (IO ())) | Stop

newScheduler :: TimeInterface -> IO Scheduler
newScheduler ti = do
    (i, o) <- U.newChan
    return $ Scheduler (sched i) (go i o dlEmpty mempty) (U.writeChan i Stop)
  where
    sched i s = getTime ti >>= enqueue i s

    enqueue i (Scheduled Nothing Nothing m) t = do
      U.writeChan i (Run $ Delayed t m) >> return Queued
    enqueue i (Scheduled (Just after) Nothing m) _ = do
      U.writeChan i (Run $ Delayed after m) >> return Queued
    enqueue i (Scheduled Nothing (Just before) m) t =
      if t < before
        then U.writeChan i (Run $ Delayed t m) >> return Queued
        else return TooLate
    enqueue i (Scheduled (Just after) (Just before) m) t
      | t < before = U.writeChan i (Run $ Delayed after m)
           >> return Queued
      | otherwise = return TooLate

    go i o dl running = do
        now <- getTime ti
        let (befores, afters) = dlSplitAfter now dl
        as <- mapM (async . dAction) $ unDelayedList befores
        let untilNextTick = maybe maxBound
               (nominalDiffTimeToMicroseconds . flip diffUTCTime now) $
               firstTime afters
        mNewDel <- readChanTimeout i o untilNextTick
        stillRunning <- filterM (fmap isNothing . poll) $ running ++ as
        case mNewDel of
            Tick -> go i o afters stillRunning
            Run newDel -> go i o (dlInsert newDel afters) stillRunning
            Stop -> mapM_ wait stillRunning

    readChanTimeout inChan outChan to = bracket
        (async $ (delay ti) to >> U.writeChan inChan Tick)
        cancel
        (\_ -> U.readChan outChan)

    firstTime (DelayedList []) = Nothing
    firstTime (DelayedList (d : _ds)) = Just $ dUntil d
