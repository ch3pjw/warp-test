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
  let actor = newActor getCurrentTime
  withAsync (mailer ms store (U.readChan o)) $ \a -> forever $ do
    putStrLn "Command pls: s <email>, v <uuid>, u <uuid>"
    input <- getLine
    case input of
      's':' ':email -> let e = Text.pack email in
        aSubmitEmailAddress actor e store (mockEmailToUuid e)
      'v':' ':uuid -> parseUuidThen (\u -> aVerify actor store u) uuid
      'u':' ':uuid -> parseUuidThen (\u -> aUnsubscribe actor store u) uuid
      'g':' ':uuid -> parseUuidThen (getAndShowState store) uuid
      _ -> putStrLn "Narp, try again"
  where
    parseUuidThen f uuid = maybe (putStrLn "rubbish uuid") f $ UUID.fromString uuid
