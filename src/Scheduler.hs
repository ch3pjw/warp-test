{-# LANGUAGE DeriveFunctor #-}

module Scheduler where

import Prelude hiding (fail)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (async, cancel)
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Exception (bracket)
import Control.Monad.Fail (MonadFail, fail)
import Data.List (insertBy, partition)
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

newtype DelayedList a = DelayedList {unDelayedList :: [Delayed a]} deriving Show

dlEmpty :: DelayedList a
dlEmpty = DelayedList []

dlSingleton :: Delayed a -> DelayedList a
dlSingleton = flip dlInsert dlEmpty

dlInsert :: Delayed a -> DelayedList a -> DelayedList a
dlInsert d = DelayedList . insertBy compareDelayedUntils d . unDelayedList

dlLength :: DelayedList a -> Int
dlLength = length . unDelayedList

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
  }

newScheduler :: TimeInterface -> IO Scheduler
newScheduler ti = do
    (i, o) <- U.newChan
    return $ Scheduler (sched i) (go i o dlEmpty)
  where
    sched i s = getTime ti >>= enqueue i s

    enqueue i (Scheduled Nothing Nothing m) t = do
      U.writeChan i (Just $ Delayed t m) >> return Queued
    enqueue i (Scheduled (Just after) Nothing m) _ = do
      U.writeChan i (Just $ Delayed after m) >> return Queued
    enqueue i (Scheduled Nothing (Just before) m) t =
      if t < before
        then U.writeChan i (Just $ Delayed t m) >> return Queued
        else return TooLate
    enqueue i (Scheduled (Just after) (Just before) m) t
      | t < before = U.writeChan i (Just $ Delayed after m) >> return Queued
      | otherwise = return TooLate

    go i o dl = do
        now <- getTime ti
        let (befores, afters) = dlSplitAfter now dl
        mapM_ (forkIO . dAction) $ unDelayedList befores
        let untilNextTick = maybe maxBound
               (nominalDiffTimeToMicroseconds . flip diffUTCTime now) $
               firstTime afters
        mNewDel <- readChanTimeout i o untilNextTick
        case mNewDel of
            Nothing -> go i o afters
            Just newDel -> go i o $ dlInsert newDel afters

    readChanTimeout inChan outChan to = bracket
        (async $ (delay ti) to >> U.writeChan inChan Nothing)
        cancel
        (\_ -> U.readChan outChan)

    firstTime (DelayedList []) = Nothing
    firstTime (DelayedList (d : _ds)) = Just $ dUntil d
