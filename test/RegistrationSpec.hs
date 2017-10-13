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
import Data.Time.Clock (addUTCTime)
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
    describe "the subscription handler" $ do
      context "when the email service is running" $ do
        it "should not let me verify a non-existant email address" $
          const pending

        beforeWith beforeDoASub $
          context "once I have submitted my email address" $ do
            it "should have sent me an email and be waiting for my click" $
              \(clock, actor, store, uo, eo, uuid) -> do
                state <- sPoll store uuid
                state `shouldSatisfy` userStateEmail "paul@concertdaw.co.uk"
                state `shouldSatisfy`
                    userStateVerification (Pending $ plusTimeout 0)

            it "should resend me a verification email if I submit again" $
              \(clock, actor, store, uo, eo, uuid) -> do
                clockSetTime clock 42
                uuid' <- subAndGetEmail actor store eo
                uuid' `shouldBe` uuid
                state <- sPoll store uuid
                state `shouldSatisfy` userStateEmail "paul@concertdaw.co.uk"
                state `shouldSatisfy`
                    userStateVerification (Pending $ plusTimeout 42)

            it "should register me as verified when I respond to the email" $
              \(clock, actor, store, uo, eo, uuid) -> do
                aVerify actor store uuid
                state <- sPoll store uuid
                state `shouldSatisfy` userStateEmail "paul@concertdaw.co.uk"
                state `shouldSatisfy` userStateVerification Verified

            it "should reject my verification if it is tardy" $
              \(clock, actor, store, uo, eo, uuid) -> do
                clockSetTime clock $ DateTime.toSeconds $ plusTimeout 2
                aVerify actor store uuid
                state <- sPoll store uuid
                state `shouldSatisfy`
                    userStateVerification (Pending $ plusTimeout 0)

            it "should accept a resubmission after rejecting my verification" $
              \(clock, actor, store, uo, eo, uuid) -> do
                clockSetTime clock $ DateTime.toSeconds $ plusTimeout 2
                aVerify actor store uuid
                uuid' <- subAndGetEmail actor store eo
                aVerify actor store uuid
                state <- sPoll store uuid
                state `shouldSatisfy` userStateVerification Verified

            it "should discard my email address when I unsubscribe" $
              checkUnsubscribed
        beforeWith (beforeDoASub >=> beforeDoAVerify) $
          context "once I have verified my email address" $ do
            it "should send me a confirmation email if I verify again" $
              \(clock, actor, store, uo, eo, uuid) -> do
                aSubmitEmailAddress actor store "paul@concertdaw.co.uk"
                uuid' <- checkInbox eo "paul@concertdaw.co.uk" ConfirmationEmail
                uuid' `shouldBe` uuid

            it "should discard my email address when I unsubscribe" $
              checkUnsubscribed

        beforeWith (beforeDoASub >=> beforeDoAVerify >=> beforeDoUnsub) $ do
          context "once I have unsubscribed" $ do
            it "should handle a subsequent unsubscribe" $
              checkUnsubscribed
            it "should behave exactly as if I had never registered" $
              const pending

      context "when the email service is idle" $ do
        it "should still accept submissions, then email on service start" $
          \_ -> do
            pending
        it "should still accept resubmissions, then email on service start" $
          \_ -> do
            pending
        it "should still accept pending verifications" $
          \_ -> do
            pending
        it "should still accept unsubscriptions" $
          \_ -> do
            pending
  where
    subAndGetEmail a s eo = do
        aSubmitEmailAddress a s "paul@concertdaw.co.uk"
        checkInbox eo "paul@concertdaw.co.uk" VerificationEmail
    checkUnsubscribed (c, a, s, uo, eo, u) = do
        aUnsubscribe a s u
        state <- sPoll s u
        state `shouldBe` initialUserState
    beforeDoASub (c, a, s, uo, eo) = do
        uuid <- subAndGetEmail a s eo
        return (c, a, s, uo, eo, uuid)
    beforeDoAVerify x@(c, a, s, uo, eo, u) = aVerify a s u >> return x
    beforeDoUnsub x@(c, a, s, uo, eo, u) = aUnsubscribe a s u >> return x



plusTimeout :: Integer -> DateTime
plusTimeout = addUTCTime verificationTimeout . DateTime.fromSeconds


-- | Predicate for checking UserState verificationState
userStateVerification :: VerificationState -> UserState -> Bool
userStateVerification vs us = usVerificationState us == vs

userStateEmail :: EmailAddress -> UserState -> Bool
userStateEmail e us = usEmailAddress us == e


type ChanPair a = (U.InChan a, U.OutChan a)

data Clock = Clock
  { clockGetTime :: IO DateTime
  , clockSetTime :: Integer -> IO ()
  , clockAdvance :: Integer -> IO ()
  }

newClock :: IO Clock
newClock = do
    mVar <- newMVar 0
    return $ Clock
      (withMVar mVar $ return . DateTime.fromSeconds)
      (modifyMVar_ mVar . const . return)
      (\i -> modifyMVar_ mVar $ return . (+i))


testContext ::
  ((Clock, Actor, Store, U.OutChan (Maybe UUID), U.OutChan Email) -> IO ()) ->
  IO ()
testContext spec = do
  clock <- newClock
  store <- newStore
  uo <- sGetNotificationChan store
  uo' <- sGetNotificationChan store
  (ei, eo) <- U.newChan
  let actor = newActor "NaCl" $ clockGetTime clock
  a <- async $ reactivelyRunAction
      (tsMockSendEmails (aGetTime actor) ei) store (U.readChan uo')
  -- FIXME: want better shutdown than cancel:
  link a
  bracket
    (return (clock, actor, store, uo, eo))
    (const $ sSendShutdown store >> wait a)
    spec

uuid1 = uuidFromInteger 1


seconds :: (RealFrac a, Integral b) => a -> b
seconds n = truncate $ n * 1e6


timeout :: (RealFrac a) => a -> IO b -> IO b
timeout n a = Timeout.timeout (seconds n) a >>= maybe (fail "timed out") return



data Email = Email EmailAddress EmailType UUID deriving (Show, Eq)

-- | Times out if we don't get the expected "email"
checkInbox ::
    U.OutChan Email -> EmailAddress -> EmailType -> IO UUID
checkInbox eo ea et =
    (timeout 0.1 $ U.readChan eo) >>= checkAndGet
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
