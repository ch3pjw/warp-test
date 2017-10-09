{-# LANGUAGE OverloadedStrings #-}

module RegistrationSpec where

import Test.Hspec
import Test.HUnit ((@?=))

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Error.Util (note)
import Control.Monad
import Data.DateTime (getCurrentTime)
import Data.UUID (UUID)
import System.Timeout

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

  before setupForTest $ do
    describe "subscription handling" $ do
      it "should send me an email when I click subscribe" $
        \(store, uo, (ei, eo)) -> do
          withAsync (
            reactivelyRunAction (tsMockSendEmails ei) store (U.readChan uo)) $
              \a -> do
                submitEmailAddress "paul@concertdaw.co.uk" store uuid1
                uuid <- checkInbox eo "paul@concertdaw.co.uk" VerificationEmail
                -- uuid `shouldBeT` uuid1
                case uuid of
                  Left s -> fail s
                  Right u -> if u == uuid1 then return () else fail (show u)



shouldBeT :: (Traversable t, Eq a, Show a) => t a -> a -> IO ()
shouldBeT ta a = void . sequence $ (@?= a) <$> ta


type ChanPair a = (U.InChan a, U.OutChan a)


setupForTest :: IO (Store, U.OutChan (Maybe UUID), ChanPair Email)
setupForTest = do
  store <- newStore
  (,,) <$> return store <*> sGetNotificationChan store <*> U.newChan

uuid1 = uuidFromInteger 1


seconds n = truncate $ n * 1e6

data Email = Email EmailAddress EmailType UUID deriving (Show, Eq)

-- | Times out if we don't get the expected "email"
checkInbox ::
    U.OutChan Email -> EmailAddress -> EmailType -> IO (Either String UUID)
checkInbox o ea et = do
    maybeEmail <- timeout (seconds 1) $ U.readChan o
    return $ note "timed out" maybeEmail >>= checkAndGet
  where
    checkAndGet (Email a t u)
        | (a, t) == (ea, et) = Right u
        | otherwise = Left $ "Bad email: " ++ show (a, t)


mockSendEmails :: U.InChan Email -> Action UserEvent
mockSendEmails i u s =
    mapM sendEmail . condenseConsecutive $ usPendingEmails s
  where
    sendEmail emailType = do
        U.writeChan i $ Email addr emailType u
        return $ Emailed emailType
    addr = usEmailAddress s

tsMockSendEmails :: U.InChan Email -> Action (TimeStamped UserEvent)
-- FIXME: this should take a source detailing what the time should be, I
-- think...
tsMockSendEmails i = timeStampedAction getCurrentTime (mockSendEmails i)
