{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async
import Control.Applicative
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString as BS
import Data.DateTime (getCurrentTime)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Database.Persist.Postgresql as DB
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.HttpAuth (basicAuth)
import System.Envy (FromEnv, fromEnv, env, decodeEnv)

import Registration
import ReadView
import Types (Password(..), EnvToggle(..))
import Mailer
import Lib
import Router
import Templates
import Middleware (forceTls, prettifyError')


data ServerConfig = ServerConfig
  { scPort :: Warp.Port
  , scAllowInsecure :: Bool
  , scDomain :: Text
    -- The username and password that we will come in with when querying our API
    -- for a list of all registered email addresses:
  , scUsername :: BS.ByteString
  , scPassword :: Password BS.ByteString
  } deriving (Show)

instance FromEnv ServerConfig where
    fromEnv = ServerConfig
      <$> env "PORT"
      <*> (unEnvToggle <$> env "ALLOW_INSECURE")
      <*> env "DOMAIN"
      <*> env "REGISTRATION_USERNAME"
      <*> env "REGISTRATION_PASSWORD"


main :: IO ()
main = do
    serverConfig <- decodeEnv'
    smtpSettings <- decodeEnv'
    senderAddr <- decodeEnv'
    regConfig <- decodeEnv'

    -- Do a bunch of initialisation:
    pool <- runNoLoggingT (DB.createPostgresqlPool (DB.pgConnStr $ rcDatabaseConfig regConfig) 2)
    store <- newDBStore pool
    o1 <- sGetNotificationChan store
    o2 <- sGetNotificationChan store
    let actor = newActor (rcUuidSalt regConfig) getCurrentTime

    let authMiddleware = buildAuth
          (scUsername serverConfig) (scPassword serverConfig)
    let icp = interestedCollectionPost actor store
    let ir = interestedResource actor store
    let ig = interestedCollectionGet pool
    let errHandler = prettifyError' $ templatedErrorTransform errorTemplate
    let app = errHandler $ dispatch $ root icp ir ig authMiddleware
    let genEmail = generateEmail senderAddr
          (formatVLink $ scDomain serverConfig)
          (formatULink $ scDomain serverConfig)

    withAsync (viewWorker pool (U.readChan o2)) $ \viewWorkerAsync -> do
        link viewWorkerAsync
        withAsync (mailer genEmail smtpSettings store (U.readChan o1)) $ \mailerAsync -> do
            link mailerAsync
            if (scAllowInsecure serverConfig)
            then Warp.run (scPort serverConfig) app
            else Warp.run (scPort serverConfig) $ forceTls app


decodeEnv' :: (FromEnv a) => IO a
decodeEnv' = decodeEnv >>= either fail return


formatVLink :: Text -> UUID -> Text
formatVLink = formatActionLink "verify"


formatULink :: Text -> UUID -> Text
formatULink = formatActionLink "unsubscribe"


formatActionLink :: Text -> Text -> UUID -> Text
formatActionLink verb domain uuid =
    "https://" <> domain <> "/interested/" <> UUID.toText uuid
    <> "?action=" <> verb


buildAuth :: BS.ByteString -> Password BS.ByteString -> Wai.Middleware
buildAuth username password = basicAuth isAllowed "Concert API"
  where
    isAllowed u p
      | u == username && p == p' = pure True
      | otherwise = pure False
    p' = unPassword password


root
  :: Wai.Application -> (Text -> Wai.Application) -> Wai.Application
  -> Wai.Middleware -> Endpoint
root icp ir ig authMiddleware =
  getEp (redir "interested") <|>
  childEps
    [ ("david", getEp $ githubRedir "foolswood")
    , ("paul", getEp $ githubRedir "ch3pjw")
    , ("screen.css", getEp $ screenCss)
    , ("api", authMiddleware <$> childEps
        [ ("interested", getEp ig)
        , previews]
      )
    , ("interested"
      , getEp interestedSubmissionGet <|>
        postEp icp <|>
        childEp (getEp . ir)
      )
    ]
  where
    previews = generatePreviews
      [ ("Verification sent", submissionResponse "name@example.com")
      , ("Verified successully", verificationResponse)
      , ("Unsubscribed successfully", unsubscriptionResponse)
      ]
