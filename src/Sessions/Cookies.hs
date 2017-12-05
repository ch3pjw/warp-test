{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Sessions.Cookies where

import Data.Aeson (encode, decode)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock
  (NominalDiffTime, getCurrentTime, secondsToDiffTime, addUTCTime)
import Web.ClientSession (Key, encryptIO)
import qualified Web.Cookie as WC

import Events (SessionEvent, AccountEvent, UuidFor, TimeStamped)

cookieLifeSpan :: NominalDiffTime
cookieLifeSpan = fromRational . toRational . secondsToDiffTime $
  28 * 24 * 60 * 60

data SessionCookie
  = SessionCookie
  { scVersion :: Int
  , scSessionUuid :: UuidFor (TimeStamped SessionEvent)
  , scAccountUuid :: UuidFor (TimeStamped AccountEvent)
  } deriving (Eq, Show)

deriveJSON (aesonPrefix camelCase) ''SessionCookie

sessionCookie
  :: UuidFor (TimeStamped SessionEvent)
  -> UuidFor (TimeStamped AccountEvent)
  -> SessionCookie
sessionCookie sUuid' aUuid' = SessionCookie 0 sUuid' aUuid'

-- | Generates a new, uniquely encrypted SetCookie, with a calculated exipry
--   time determined by the current system time.
sessionSetCookie :: Key -> SessionCookie -> IO WC.SetCookie
sessionSetCookie key cookieData = do
  cipherText <- encryptIO key . toStrict . encode $ cookieData
  t <- getCurrentTime
  return $ WC.def
    { WC.setCookieName = "sessionCookie"
    , WC.setCookieValue = cipherText
    , WC.setCookiePath = Just "/"
    , WC.setCookieSecure = True
    , WC.setCookieExpires = Just $ addUTCTime cookieLifeSpan t
   }

decodeSessionCookie :: ()
decodeSessionCookie = undefined

acceptCookiePolicySetCookie :: WC.SetCookie
acceptCookiePolicySetCookie = WC.def
  { WC.setCookieName = "acceptCookiePolicy"
  , WC.setCookieValue = "yes"
  }
