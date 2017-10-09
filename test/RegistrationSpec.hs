{-# LANGUAGE OverloadedStrings #-}

module RegistrationSpec where

import Test.Hspec
import Test.HUnit ((@?=))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Concurrent.MVar
import Control.Error.Util (note)
import Control.Exception (bracket)
import Control.Monad
import Data.DateTime (DateTime)
import qualified Data.DateTime as DateTime
import Data.UUID (UUID)
import qualified System.Timeout as Timeout

import Eventful (uuidFromInteger)

import Registration


spec :: Spec
spec = do
  describe "condenseConsecutive" $ do
    it "should handle empty lists" $
      condenseConsecutive [] `shouldBe` ([] :: [Int])

    it "should handle singleton lists" $
      condenseConsecutive [1] `shouldBe` [1]

    it "shouldn't affect a list of unique items" $
      condenseConsecutive [1, 2, 3, 4] `shouldBe` [1, 2, 3, 4]

    it "should convert neighbouring duplicates into a single item" $
      condenseConsecutive [1, 1, 2, 3, 3, 3, 4] `shouldBe` [1, 2, 3, 4]

  around testContext $ do
    describe "subscription handling" $ do
      beforeWith (\(actor, store, eo) -> do
          aSubmitEmailAddress actor "paul@concertdaw.co.uk" store uuid1
          uuid <- checkInbox eo "paul@concertdaw.co.uk" VerificationEmail
          return (actor, store, eo, uuid)) $
        context "once I have submitted my email address" $ do
          it "should have sent me an email and be waiting for my click" $
              \(actor, store, eo, uuid) -> do
                uuid `shouldBe` uuid1
                state <- sPoll store uuid
                -- FIXME: why have so many things fetched the time?
                state `shouldBe` UserState
                    (Pending $ DateTime.fromSeconds 20) [] "paul@concertdaw.co.uk"

          it "should register me as verified when I respond to the verfn email" $
              \(actor, store, eo, uuid) -> do
                aVerify actor store uuid
                state <- sPoll store uuid
                state `shouldBe` UserState Verified [] "paul@concertdaw.co.uk"

          it "should discard my email address when I unsubscribe" $
              \(actor, store, eo, uuid) -> do
              -- FIXME: check from multiple states:
                aVerify actor store uuid
                aUnsubscribe actor store uuid
                state <- sPoll store uuid
                state `shouldBe` initialUserState



type ChanPair a = (U.InChan a, U.OutChan a)

newtype Clock = Clock {clockGetTime :: IO DateTime}

newClock :: IO Clock
newClock = do
    mVar <- newMVar 0
    return $ Clock $ getTime mVar
  where
    getTime mVar = modifyMVar mVar $ \n ->
      return (n + 1, DateTime.fromSeconds n)


testContext :: ((Actor, Store, U.OutChan Email) -> IO ()) -> IO ()
testContext spec = do
  clock <- newClock
  store <- newStore
  uo <- sGetNotificationChan store
  (ei, eo) <- U.newChan
  let actor = newActor $ clockGetTime clock
  a <- async $ reactivelyRunAction
      (tsMockSendEmails (aGetTime actor) ei) store (U.readChan uo)
  -- FIXME: want better shutdown than cancel:
  link a
  bracket (return (actor, store, eo)) (const $ cancel a) spec

uuid1 = uuidFromInteger 1


seconds :: (RealFrac a, Integral b) => a -> b
seconds n = truncate $ n * 1e6


timeout :: (RealFrac a) => a -> IO b -> IO b
timeout n a = Timeout.timeout (seconds n) a >>= maybe (fail "timed out") return



data Email = Email EmailAddress EmailType UUID deriving (Show, Eq)

-- | Times out if we don't get the expected "email"
checkInbox ::
    U.OutChan Email -> EmailAddress -> EmailType -> IO UUID
checkInbox o ea et =
    (timeout 1 $ U.readChan o) >>= checkAndGet
  where
    checkAndGet (Email a t u)
        | (a, t) == (ea, et) = return u
        | otherwise = fail $ "Bad email: " ++ show (a, t)


mockSendEmails :: U.InChan Email -> Action UserEvent
mockSendEmails i u s =
    mapM sendEmail . condenseConsecutive $ usPendingEmails s
  where
    sendEmail emailType = do
        U.writeChan i $ Email addr emailType u
        return $ Emailed emailType
    addr = usEmailAddress s

tsMockSendEmails :: IO DateTime -> U.InChan Email -> Action (TimeStamped UserEvent)
tsMockSendEmails getT i = timeStampedAction getT (mockSendEmails i)
