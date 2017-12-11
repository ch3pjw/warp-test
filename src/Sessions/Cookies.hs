{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Sessions.Cookies where

import Blaze.ByteString.Builder (toByteString)
import Control.Error.Util (note)
import Data.Aeson (encode, decode)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Time.Clock
  (NominalDiffTime, getCurrentTime, secondsToDiffTime, addUTCTime)
import Data.DateTime (startOfTime)
import qualified Network.Wai as Wai
import qualified Network.Wai.Trans as Wai
import qualified Network.HTTP.Types as HTTP
import Web.ClientSession (Key, encryptIO, decrypt)
import qualified Web.Cookie as WC

import Events (SessionEvent, AccountEvent, TimeStamped)
import UuidFor (UuidFor)
import WaiUtils (sendResponseWithHeader)

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
    -- FIXME: "cookie" is a little redundent in this name
    { WC.setCookieName = "sessionCookie"
    , WC.setCookieValue = cipherText
    , WC.setCookiePath = Just "/"
    , WC.setCookieSecure = True
    , WC.setCookieExpires = Just $ addUTCTime cookieLifeSpan t
   }

retrieveSessionCookie :: Key -> Wai.Request -> Either String SessionCookie
retrieveSessionCookie key req =
  note "No cookie headers" (lookup HTTP.hCookie $ Wai.requestHeaders req) >>=
  return . WC.parseCookies >>=
  -- FIXME: "sessionCookie" should be a global value
  note "Session cookie not present" . lookup "sessionCookie" >>=
  note "Failed to decrypt session cookie" . decrypt key >>=
  note "Failed to JSON decode session cookie" . decode . fromStrict

maybeWithSessionCookie
  :: (Monad m)
  => Key -> Wai.ApplicationT m -> (SessionCookie -> Wai.ApplicationT m)
  -> Wai.ApplicationT m
maybeWithSessionCookie key defApp withScApp req sendResponse =
    either
      (const defApp)
      withScApp
      (retrieveSessionCookie key req)
    req sendResponse

acceptCookiePolicySetCookie :: WC.SetCookie
acceptCookiePolicySetCookie = WC.def
  { WC.setCookieName = "acceptCookiePolicy"
  , WC.setCookieValue = "yes"
  }

expireCookie :: ByteString -> WC.SetCookie
expireCookie name = WC.def
  { WC.setCookieName = name
  , WC.setCookieExpires = Just startOfTime
  }

sendResponseWithCookie
  :: WC.SetCookie -> (Wai.Response -> t) -> Wai.Response -> t
sendResponseWithCookie sc = sendResponseWithHeader "Set-Cookie" bytes
  where
    bytes = toByteString $ WC.renderSetCookie sc
