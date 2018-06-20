{-# LANGUAGE OverloadedStrings #-}

module Utils
  ( postgresUp
  , retry
  , withDockerD
  ) where

import Database.PostgreSQL.Simple
import Control.Concurrent
import Control.Exception (SomeException(..), try, bracket)
import System.Process (readProcess, callProcess)

retry :: IO a -> IO a
retry action = try action >>= either
    (\(SomeException _) -> threadDelay 1000000 >> retry action)
    return

postgresUp :: IO Connection
postgresUp = connect ConnectInfo { connectHost = "localhost"
                                 , connectPort = 8000
                                 , connectUser = "postgres"
                                 , connectPassword = "password"
                                 , connectDatabase = "eventful_test"}

withDockerD :: [String] -> IO a -> IO a
withDockerD args io = bracket
  (readProcess "docker" (["run", "-d"] ++ args) "")
  (\containerId -> callProcess "docker" ["kill", filter (/= '\n') containerId])
  (const io)

