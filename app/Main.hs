{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad
import Data.DateTime (getCurrentTime)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import System.Environment (getEnv)

import Lib
import Registration
import Mailer

main :: IO ()
-- main = getEnv "PORT" >>= someFunc . read
main = do
  ms <- settingsFromEnv UUID.toText UUID.toText
  store <- newStore
  o <- sGetNotificationChan store
  let actor = newActor "NaCl" getCurrentTime
  go ms store actor o
  where
    parseUuidThen f uuid = maybe (putStrLn "rubbish uuid") f $ UUID.fromString uuid
    go ms store actor o =
        withAsync (mailer ms store (U.readChan o)) $
          \a -> do
            putStrLn "Command pls: s <email>, v <uuid>, u <uuid>, g <uuid>, q"
            input <- getLine
            case input of
              's':' ':email -> let e = Text.pack email in
                  aSubmitEmailAddress actor store e >> go ms store actor o
              'v':' ':uuid ->
                  parseUuidThen (\u -> aVerify actor store u) uuid >>
                  go ms store actor o
              'u':' ':uuid ->
                  parseUuidThen (\u -> aUnsubscribe actor store u) uuid >>
                  go ms store actor o
              'g':' ':uuid -> parseUuidThen (getAndShowState store) uuid
              'q':_ -> sSendShutdown store
              _ -> putStrLn "Narp, try again" >> go ms store actor o
