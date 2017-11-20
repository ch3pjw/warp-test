{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent.Async
import Control.Applicative
import qualified Control.Concurrent.Chan.Unagi as U
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString as BS
import Data.DateTime (getCurrentTime)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Database.Persist.Postgresql as DB
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Trans as Wai
import Network.Wai.Middleware.HttpAuth (basicAuth)
import System.Envy (FromEnv, fromEnv, env, envMaybe, decodeEnv)

import Registration
import ReadView
import Store (newDBStore, sGetNotificationChan)
import Types (Password(..), EnvToggle(..))
import Events (UuidFor(..))
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
      <*> (maybe False unEnvToggle <$> envMaybe "ALLOW_INSECURE")
      <*> env "DOMAIN"
      <*> env "REGISTRATION_USERNAME"
      <*> env "REGISTRATION_PASSWORD"


main :: IO ()
main = do
    serverConfig <- decodeEnv'
    smtpSettings <- decodeEnv'
    senderAddr <- decodeEnv'
    regConfig <- decodeEnv'
    static <- decodeEnv'

    -- Do a bunch of initialisation:
    pool <- runNoLoggingT (DB.createPostgresqlPool (DB.pgConnStr $ rcDatabaseConfig regConfig) 2)
    store <- newDBStore pool
    o <- sGetNotificationChan store
    let actor = newEmailActor (rcUuidSalt regConfig) getCurrentTime store

    let authMiddleware = buildAuth
          (scUsername serverConfig) (scPassword serverConfig)
    let icp = interestedCollectionPost actor
    let ir = interestedResource actor
    let ig = interestedCollectionGet pool
    let errHandler = prettifyError' $ templatedErrorTransform errorTemplate
    -- FIXME: runReaderT' for authMiddleware:
    let app = wrapApp static $ errHandler $ dispatch $ root icp ir ig (Wai.liftMiddleware (runReaderT' static) authMiddleware)
    let genEmail = generateEmail senderAddr
          (formatVLink $ scDomain serverConfig)
          (formatULink $ scDomain serverConfig)

    let getWait = U.readChan <$> sGetNotificationChan store

    withAsync (runWorkers [userStateReadView] pool getWait) $ \viewWorkerAsync -> do
        link viewWorkerAsync
        withAsync (mailer genEmail smtpSettings (fmap UuidFor <$> U.readChan o) store) $ \mailerAsync -> do
            link mailerAsync
            if (scAllowInsecure serverConfig)
            then Warp.run (scPort serverConfig) app
            else Warp.run (scPort serverConfig) $ forceTls app

wrapApp :: StaticResources -> Appy -> Wai.Application
wrapApp static a = Wai.runApplicationT (flip runReaderT static) a


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


type WebbyMonad = ReaderT StaticResources IO
type Appy = Wai.ApplicationT WebbyMonad


runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT

root
  :: Appy -> (Text -> Appy)
  -> Appy -> Wai.MiddlewareT WebbyMonad -> Endpoint WebbyMonad
root icp ir ig authMiddleware =
  getEp (redir "interested") <|>
  childEps
    [ ("david", getEp $ githubRedir "foolswood")
    , ("paul", getEp $ githubRedir "ch3pjw")
    , ("about", getEp $ htmlResponse aboutUs)
    , ("company", getEp $ htmlResponse companyInfo)
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
