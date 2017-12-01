module Util
  ( Clock, newClock
  , clockGetTime
  , clockSetTime
  , clockAdvance
  , clockInterface
  ) where

import Control.Monad (void)
import Control.Concurrent.MVar
import Data.DateTime (DateTime, fromSeconds)

import Scheduler
  ( TimeInterface(..), Delayed(..), unDelayedList, dlEmpty, dlInsert
  , dlSplitAfter)

data Clock = Clock
  { clockGetTime :: IO DateTime
  , clockSetTime :: Int -> IO ()
  , clockAdvance :: Int -> IO ()
  , clockDelay :: Int -> IO ()
  }

newClock :: IO Clock
newClock = do
    tMVar <- newMVar (0 :: Int)
    pendingMVar <- newMVar dlEmpty
    let getT = withMVar tMVar $ return . fromSeconds . toInteger
    let setT i = do
          _ <- takeMVar tMVar
          runRequired pendingMVar i
          putMVar tMVar i
    let advT i = do
          curI <- takeMVar tMVar
          let newI = curI + i
          runRequired pendingMVar newI
          putMVar tMVar newI
    let delT ms = do
          let i = ms `div` 1000000
          waitForMe <- newEmptyMVar
          modifyMVar_ pendingMVar $
              return . dlInsert (Delayed (mkT i) $ putMVar waitForMe ())
          void $ takeMVar waitForMe
    return $ Clock getT setT advT delT
  where
    mkT = fromSeconds . toInteger
    runRequired pendingMVar i = do
        pending <- takeMVar pendingMVar
        let (toRun, _stillPending) = dlSplitAfter (mkT i) pending
        mapM_ dAction $ unDelayedList toRun
        putMVar pendingMVar pending

clockInterface :: Clock -> TimeInterface
clockInterface c = TimeInterface (clockGetTime c) (clockDelay c)
