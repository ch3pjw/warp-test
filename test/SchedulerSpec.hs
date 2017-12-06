{-# LANGUAGE OverloadedStrings #-}

module SchedulerSpec where

import Test.Hspec

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad (void)
import qualified Data.DateTime as DateTime
import Data.Time.Clock (UTCTime)
import System.Timeout (timeout)

import Scheduler
  ( newScheduler, schedule, runScheduler, stopScheduler
  , actNow, actBefore, actAfter, actBetween
  , QueuedStatus(..))

import Util (newClock, clockSetTime, clockInterface)

t0, t1 :: UTCTime
t0 = DateTime.fromSeconds 0
t1 = DateTime.fromSeconds 1

shouldTimeout :: (Eq a, Show a) => IO a -> IO ()
shouldTimeout m = do
    maybeResult <- timeout 30000 m
    maybeResult `shouldBe` Nothing

shouldBeWithTimeout :: (Eq a, Show a) => IO a -> a -> IO ()
shouldBeWithTimeout m expected = do
    maybeResult <- timeout 30000 m
    maybeResult `shouldBe` Just expected

shouldFailTake :: (Eq a, Show a) => MVar a -> IO ()
shouldFailTake v = shouldTimeout $ takeMVar v

shouldSucceedTakeExactlyOnce :: (Eq a, Show a) => MVar a -> IO a
shouldSucceedTakeExactlyOnce v = do
    maybeResult <- timeout 30000 $ takeMVar v
    maybeResult `shouldNotBe` Nothing
    maybeResult' <- timeout 30000 $ takeMVar v
    maybeResult' `shouldBe` Nothing
    maybe (error "impossible nothing!") return maybeResult

spec :: Spec
spec = do
  describe "Scheduled" $ do
    describe "actBetween constructor" $ do
      it "should fail if given an impossible time window" $
        actBetween t1 t0 () `shouldBe` Nothing

  around testContext $ do
    context "scheduler" $ do
      describe "enqueing a request to actNow" $ do
        it "should result in Queued status" $
          \(_, sched) -> do
            status <- schedule sched $ actNow $ error "should never run"
            status `shouldBe` Queued

        it "should never execute the given action when stopped" $
          \(_, sched) -> do
            v <- newEmptyMVar
            _status <- schedule sched $ actNow $ putMVar v '2'
            maybeResult <- timeout 30000 $ takeMVar v
            maybeResult `shouldBe` Nothing

        it "should execute the action exactly once immediately when running" $
          \(_, sched) -> do
            v <- newEmptyMVar
            _status <- schedule sched $ actNow $ putMVar v '3'
            withAsync (runScheduler sched) $ \_ -> do
              result <- shouldSucceedTakeExactlyOnce v
              result `shouldBe` '3'

      describe "enqueing a request to actBefore" $ do
        it "should reject the request if 'before' is in the past" $
          \(clock, sched) -> do
            clockSetTime clock 10
            status <- schedule sched $ actBefore t1 $ error "should never run"
            status `shouldBe` TooLate

        context "if successfully queued" $
          it "should execute the action exactly once immediately when running" $
            \(_, sched) -> do
              v <- newEmptyMVar
              status <- schedule sched $ actBefore t1 $ putMVar v '4'
              status `shouldBe` Queued
              withAsync (runScheduler sched) $ \_ -> do
                result <- shouldSucceedTakeExactlyOnce v
                result `shouldBe` '4'

      describe "enqueing a request to actAfter" $ do
        it "should execute the action immediately if the time to act is past" $
          \(clock, sched) -> do
            v <- newEmptyMVar
            clockSetTime clock 1
            status <- schedule sched $ actAfter t0 $ putMVar v '5'
            status `shouldBe` Queued
            withAsync (runScheduler sched) $ \_ -> do
              result <- shouldSucceedTakeExactlyOnce v
              result `shouldBe` '5'

        it "should wait to execute the action if it is not time to act" $
          \(clock, sched) -> do
            v <- newEmptyMVar
            status <- schedule sched $ actAfter t1 $ putMVar v '6'
            status `shouldBe` Queued
            withAsync (runScheduler sched) $ \_ -> do
              shouldFailTake v
              clockSetTime clock 3
              result <- shouldSucceedTakeExactlyOnce v
              result `shouldBe` '6'

      describe "shutdown" $ do
        it "should exit the runner thread" $
          \(_, sched) -> do
            withAsync (runScheduler sched) $ \a -> do
              stopScheduler sched
              wait a `shouldBeWithTimeout` ()

        it "should wait for running threads to exit before returning control" $
          \(_, sched) -> do
            v <- newEmptyMVar
            _status <- schedule sched $ actNow $ void $ takeMVar v
            withAsync (runScheduler sched) $ \a -> do
              stopScheduler sched
              shouldTimeout $ wait a
              putMVar v '7'
              wait a `shouldBeWithTimeout` ()

  where
    testContext specFunc = do
      clock <- newClock
      sched <- newScheduler (clockInterface clock)
      specFunc (clock, sched)
