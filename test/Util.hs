module Util
  ( Clock, newClock
  , clockGetTime
  , clockSetTime
  , clockAdvance
  ) where

import Control.Concurrent.MVar
import Data.DateTime (DateTime, fromSeconds)

data Clock = Clock
  { clockGetTime :: IO DateTime
  , clockSetTime :: Integer -> IO ()
  , clockAdvance :: Integer -> IO ()
  }

newClock :: IO Clock
newClock = do
    mVar <- newMVar 0
    return $ Clock
      (withMVar mVar $ return . fromSeconds)
      (modifyMVar_ mVar . const . return)
      (\i -> modifyMVar_ mVar $ return . (+i))
