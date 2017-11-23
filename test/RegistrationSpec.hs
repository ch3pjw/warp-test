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
import Control.Monad.IO.Class (liftIO)
import Data.DateTime (DateTime)
import qualified Data.DateTime as DateTime
import Data.Time.Clock (addUTCTime)
import Data.UUID (UUID)
import qualified System.Timeout as Timeout

import Eventful (uuidFromInteger, ExpectedPosition(AnyPosition))

import Events
  ( UuidFor(..), coerceUuidFor
  , EmailAddress
  , RegistrationEmailType(..)
  , EmailEvent(..)
  , Event, toEvent
  , TimeStamped
  )
import EventT
  ( EventT, mapEvents
  , getState, logEvents
  )
import Registration
import Store
  ( newInMemoryStore, sSendShutdown, sGetWaitUpdate, Store
  , untilNothing, reactivelyRunEventT)


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

  describe "untilNothing" $ do
    it "should execute actions until its wait function returns Nothing" $ do
      (i, o) <- U.newChan
      m <- newMVar []
      mapM_ (U.writeChan i . Just) ("hello" :: [Char])
      U.writeChan i Nothing
      untilNothing (U.readChan o) (\c -> modifyMVar_ m (return . (c:)))
      result <- takeMVar m
      result `shouldBe` "olleh"


  around testContext $ do
    describe "the subscription handler" $ do
      context "when the email service is running" $ do
        it "should not let me verify a non-existant email address" $
          \ctx ->
            let actor = tcActor ctx in do
            aVerify actor uuid1'
            state <- aPoll actor uuid1'
            state `shouldSatisfy` emailStateVerification (Unverified)

        beforeWith beforeDoASub $
          context "once I have submitted my email address" $ do
            it "should have sent me an email and be waiting for my click" $
              \(ctx, uuid) -> do
                state <- aPoll (tcActor ctx) uuid
                state `shouldSatisfy` emailStateEmail "paul@concertdaw.co.uk"
                state `shouldSatisfy`
                    emailStateVerification (Pending $ plusTimeout 0)

            it "should resend me a verification email if I submit again" $
              \(ctx, uuid) ->
                let actor = tcActor ctx in do
                clockSetTime (tcClock ctx) 42
                uuid' <- subAndGetEmail actor (tcEmailChanOut ctx)
                uuid' `shouldBe` uuid
                state <- aPoll actor uuid
                state `shouldSatisfy` emailStateEmail "paul@concertdaw.co.uk"
                state `shouldSatisfy`
                    emailStateVerification (Pending $ plusTimeout 42)

            it "should register me as verified when I respond to the email" $
              \(ctx, uuid) ->
                let actor = tcActor ctx in do
                aVerify actor uuid
                state <- aPoll actor uuid
                state `shouldSatisfy` emailStateEmail "paul@concertdaw.co.uk"
                state `shouldSatisfy` emailStateVerification Verified

            it "should reject my verification if it is tardy" $
              \(ctx, uuid) ->
                let actor = tcActor ctx in do
                clockSetTime (tcClock ctx) $ DateTime.toSeconds $ plusTimeout 2
                aVerify actor uuid
                state <- aPoll actor uuid
                state `shouldSatisfy`
                    emailStateVerification (Pending $ plusTimeout 0)

            it "should accept a resubmission after rejecting my verification" $
              \(ctx, uuid) ->
                let actor = tcActor ctx in do
                clockSetTime (tcClock ctx) $ DateTime.toSeconds $ plusTimeout 2
                aVerify actor uuid
                uuid' <- subAndGetEmail actor (tcEmailChanOut ctx)
                aVerify actor uuid
                state <- aPoll actor uuid
                state `shouldSatisfy` emailStateVerification Verified

            it "should discard my email address when I unsubscribe" $
              checkUnsubscribed
        beforeWith (beforeDoASub >=> beforeDoAVerify) $
          context "once I have verified my email address" $ do
            it "should send me a confirmation email if I verify again" $
              \(ctx, uuid) -> do
                aSubmitEmailAddress (tcActor ctx) "paul@concertdaw.co.uk"
                uuid' <- checkInbox (tcEmailChanOut ctx) "paul@concertdaw.co.uk"
                    ConfirmationEmail
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
    subAndGetEmail actor emailOutChan = do
        aSubmitEmailAddress actor "paul@concertdaw.co.uk"
        checkInbox emailOutChan "paul@concertdaw.co.uk" VerificationEmail
    checkUnsubscribed (ctx, u') =
        let actor = tcActor ctx in do
        aUnsubscribe actor u'
        state <- aPoll actor u'
        state `shouldBe` initialEmailState
    beforeDoASub ctx = do
        uuid <- subAndGetEmail (tcActor ctx) (tcEmailChanOut ctx)
        return (ctx, uuid)
    beforeDoAVerify x@(ctx, u') = aVerify (tcActor ctx) u' >> return x
    beforeDoUnsub x@(ctx, u') = aUnsubscribe (tcActor ctx) u' >> return x


data TestContext
  = TestContext
  { tcClock :: Clock
  , tcActor :: EmailActor
  , tcStore :: Store IO (TimeStamped Event)
  , tcEmailChanOut :: U.OutChan Email
  }



plusTimeout :: Integer -> DateTime
plusTimeout = addUTCTime verificationTimeout . DateTime.fromSeconds


-- | Predicate for checking EmailState verificationState
emailStateVerification :: VerificationState -> EmailState -> Bool
emailStateVerification vs emailState = esVerificationState emailState == vs

emailStateEmail :: EmailAddress -> EmailState -> Bool
emailStateEmail e emailState = esEmailAddress emailState == e


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


testContext :: (TestContext -> IO ()) -> IO ()
testContext spec = do
    clock <- newClock
    store <- newInMemoryStore :: IO (Store IO (TimeStamped Event))
    waitUpdate <- sGetWaitUpdate store
    (ei, eo) <- U.newChan
    let actor = newEmailActor "NaCl" (clockGetTime clock) store
    a <- async $ reactivelyRunEventT
        (\u -> liftToEvent $ mockSendEmails (aGetTime actor) ei u)
        waitUpdate
        store
    link a
    bracket
      (return $ TestContext clock actor store eo)
      (const $ sSendShutdown store >> wait a)
      spec
  where
    liftToEvent = mapEvents (fmap toEvent) slightlySaferEventToEmailEvent


uuid1' :: UuidFor EmailEvent
uuid1' = UuidFor $ uuidFromInteger 1


seconds :: (RealFrac a, Integral b) => a -> b
seconds n = truncate $ n * 1e6


timeout :: (RealFrac a) => a -> IO b -> IO b
timeout n a = Timeout.timeout (seconds n) a >>= maybe (fail "timed out") return



data Email =
    Email EmailAddress RegistrationEmailType (UuidFor EmailEvent)
    deriving (Show, Eq)

-- | Times out if we don't get the expected "email"
checkInbox ::
    U.OutChan Email -> EmailAddress -> RegistrationEmailType
    -> IO (UuidFor EmailEvent)
checkInbox eo ea et =
    (timeout 0.1 $ U.readChan eo) >>= checkAndGet
  where
    checkAndGet (Email a t u)
        | (a, t) == (ea, et) = return u
        | otherwise = fail $ "Bad email: " ++ show (a, t)

mockSendEmails
  :: IO DateTime -> U.InChan Email -> UUID
  -> EventT (TimeStamped EmailEvent) IO ()
mockSendEmails getT i uuid = do
    s <- getState initialEmailProjection uuid
    mapM_ (sendEmail s) $ condenseConsecutive $ esPendingEmails s
  where
    sendEmail s emailType = do
        t <- liftIO getT
        liftIO $ U.writeChan i $ Email (esEmailAddress s) emailType (UuidFor uuid)
        logEvents uuid AnyPosition [(t, EmailSentEmailEvent emailType)]
