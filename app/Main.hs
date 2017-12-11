{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent.Async
import Control.Applicative
import Control.Monad (void)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString as BS
import Data.DateTime (getCurrentTime)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Database.Persist.Postgresql as DB
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Trans as Wai
import Network.Wai.Middleware.HttpAuth (basicAuth)
import System.Envy (FromEnv, fromEnv, env, envMaybe, decodeEnv)
import qualified Web.ClientSession as ClientSession

import Events
  (Command, AccountCommand, SessionCommand, decomposeCommand, SessionEvent)
import EventT (timeStamp)
import Lib
import Middleware (forceTls, prettifyError')
import ReadView (runReadViews)
import Registration
import Router
import Scheduler
  ( Scheduler, newScheduler, schedule, runScheduler, stopScheduler
  , normalTimeInterface, Scheduled(..))
import Store (newDBStore, sGetWaitUpdate, sRunEventT)
import Templates
import Types (Password(..), EnvToggle(..))
import UuidFor (UuidFor, coerceUuidFor)
import qualified UuidFor as UuidFor
import WaiUtils (respondHtml, respondText)

import Accounts.CommandHandler (mkAccountCommandHandler)

import Sessions.CommandHandler
  ( newSessionUserActor, SessionUserActor(..), mkSessionCommandHandler
  , liftSessionEventT)
import Sessions.Cookies (maybeWithSessionCookie)
import Sessions.Pages
  (homePageContent, signInPost, signInResource, signOutResource)
import Sessions.ReadViews (activeSessionProcessManager)


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
    cookieEncryptionKey <- ClientSession.getKeyEnv "COOKIE_KEY"

    -- Do a bunch of initialisation:
    pool <- runNoLoggingT (DB.createPostgresqlPool (DB.pgConnStr $ rcDatabaseConfig regConfig) 2)
    store <- newDBStore pool
    let sessionActor = newSessionUserActor getCurrentTime store

    let authMiddleware = buildAuth
          (scUsername serverConfig) (scPassword serverConfig)
    let hp = homePage cookieEncryptionKey
    let sip = signInPost $ suaRequestSession sessionActor
    let sig = signInResource cookieEncryptionKey $ suaSignIn sessionActor
    let sog = signOutResource $ suaSignOut sessionActor
    let ig = interestedCollectionGet pool
    let errHandler = prettifyError' $ templatedErrorTransform errorTemplate
    -- FIXME: runReaderT' for authMiddleware:
    let app = wrapApp static $ errHandler $ dispatch $ root hp sip sig sog ig
          (Wai.liftMiddleware (runReaderT' static) authMiddleware)

    let getWait = sGetWaitUpdate store
    scheduler <- newScheduler normalTimeInterface
    let sessionCmdHandler =
          sRunEventT store . timeStamp getCurrentTime . liftSessionEventT
          . mkSessionCommandHandler senderAddr smtpSettings
            (formatSignInLink $ scDomain serverConfig)
          :: SessionCommand -> IO ()
    let acctCmdHandler =
          sRunEventT store . timeStamp getCurrentTime
          . mkAccountCommandHandler (coerceUuidFor . hashEmail (rcUuidSalt regConfig))
          :: AccountCommand -> IO ()
    let cmdHandler = scheduleCmds acctCmdHandler sessionCmdHandler scheduler
    let schedCmds = maybe
          (stopScheduler scheduler)
          (mapM_ cmdHandler) :: Maybe [Scheduled Command] -> IO ()

    withAsync (runReadViews [(schedCmds, activeSessionProcessManager)] pool getWait) $ \viewWorkerAsync -> do
        link viewWorkerAsync
        withAsync (runScheduler scheduler) $ \schedulerAsync -> do
            link schedulerAsync
            if (scAllowInsecure serverConfig)
            then Warp.run (scPort serverConfig) app
            else Warp.run (scPort serverConfig) $ forceTls app

scheduleCmds
  :: (AccountCommand -> IO ())
  -> (SessionCommand -> IO ())
  -> Scheduler
  -> Scheduled Command -> IO ()
scheduleCmds aHandler sHandler scheduler =
    void . schedule scheduler
    . fmap (decomposeCommand (const $ return ()) aHandler sHandler)

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

formatSignInLink :: Text -> UuidFor SessionEvent -> Text
formatSignInLink domain uuid' =
    "https://" <> domain <> "/signIn/" <> UuidFor.toText uuid'


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


homePage :: ClientSession.Key -> Appy
homePage key = maybeWithSessionCookie key
    (respondHtml $ homePageContent False) (respondText . LazyText.pack . show)

root
  :: Appy -> Appy -> (Text -> Appy)
  -> (Text -> Appy) -> Appy -> Wai.MiddlewareT WebbyMonad
  -> Endpoint WebbyMonad
root hp sip sig sog ig authMiddleware =
  -- FIXME: these arguments are getting stupid...
  getEp hp <|>
  postEp sip <|>
  childEps
    [ ("david", getEp $ githubRedir "foolswood")
    , ("paul", getEp $ githubRedir "ch3pjw")
    , ("about", getEp $ respondHtml aboutUs)
    , ("company", getEp $ respondHtml companyInfo)
    , ("screen.css", getEp $ screenCss)
    , ("api", authMiddleware <$> childEps
        [ ("interested", getEp ig)
        , previews]
      )
    , ("signIn"
      ,
        childEp (getEp . sig)
      )
    , ("signOut"
      , childEp (getEp . sog)
      )
    ]
  where
    previews = generatePreviews
      [ ("Verification sent", submissionResponse "name@example.com")
      , ("Verified successully", verificationResponse)
      , ("Unsubscribed successfully", unsubscriptionResponse)
      ]
