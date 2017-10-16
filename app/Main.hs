{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Applicative
import qualified Control.Concurrent.Chan.Unagi as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.DateTime (getCurrentTime)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.HttpAuth (basicAuth)
import System.Environment (getEnv, lookupEnv)

import Registration
import Mailer
import Lib
import Router
import Middleware (forceTls, prettifyError)

main :: IO ()
main = do
    -- Read a bunch of environment config:
    port <- read <$> getEnv "PORT"
    allowInsecure <- maybe False (not . null) <$> lookupEnv "ALLOW_INSECURE"
    domain <- Text.pack <$> getEnv "DOMAIN"
    salt <- Text.pack <$> getEnv "REGISTRATION_UUID_SALT"
    -- The username and password that we will come in with when querying our API
    -- for a list of all registered email addresses:
    username <- UTF8.fromString <$> getEnv "REGISTRATION_USERNAME"
    password <- UTF8.fromString <$> getEnv "REGISTRATION_PASSWORD"
    ms <- mailerSettingsFromEnv (formatVLink domain) (formatULink domain)
    dbConfig <- getDatabaseConfig

    -- Do a bunch of initialisation:
    store <- newDBStore dbConfig
    o <- sGetNotificationChan store
    let actor = newActor salt getCurrentTime

    let authMiddlware = buildAuth username password
    let icp = interestedCollectionPost actor store
    let ir = interestedResource actor store
    let app = prettifyError $ dispatch $ root icp ir authMiddlware

    withAsync (mailer ms store (U.readChan o)) $ \a -> do
        link a
        if allowInsecure
        then Warp.run port app
        else Warp.run port $ forceTls app


formatVLink :: Text -> UUID -> Text
formatVLink = formatActionLink "verify"


formatULink :: Text -> UUID -> Text
formatULink = formatActionLink "unsubscribe"


formatActionLink :: Text -> Text -> UUID -> Text
formatActionLink verb domain uuid =
    "https://" <> domain <> "/interested/" <> UUID.toText uuid
    <> "?action=" <> verb


buildAuth :: BS.ByteString -> BS.ByteString -> Wai.Middleware
buildAuth username password = basicAuth isAllowed "Concert API"
  where
    isAllowed u p
      | u == username && p == password = pure True
      | otherwise = pure False


root ::
  Wai.Application -> (Text -> Wai.Application) -> Wai.Middleware -> Endpoint
root icp ir authMiddleware =
  getEp (redir "interested") <|>
  childEps
    [ ("david", getEp $ githubRedir "foolswood")
    , ("paul", getEp $ githubRedir "ch3pjw")
    , ("api", authMiddleware <$> childEps
        [("interested",
          getEp interestedCollectionGet
         )]
      )
    , ("interested"
      , getEp interestedSubmissionGet <|>
        postEp icp <|>
        childEp (getEp . ir)
      )
    ]
