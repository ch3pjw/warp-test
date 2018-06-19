{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import System.IO
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid
import Data.String (IsString)
import GHC.Generics
import Data.Text
import Control.Monad
import Network.Wai
import Network.Http.Client
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.Async
import Control.Concurrent.Chan
import System.Process
import System.Timeout
import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import Test.Hspec

import Utils (retry, postgresUp, withDockerD)

signUp :: BS.ByteString -> IO ()
signUp address = withConnection (openConnection "127.0.0.1" 8080) (\c -> do

    let q = buildRequest1 $ do
                http POST "/interested"
                setAccept "*/*"
                setContentType "application/x-www-form-urlencoded"

    sendRequest c q (encodedFormBody [("email", address)])

    receiveResponse c debugHandler)

data ConcertMail =
    ConcertMail { body :: Text
    , mailFrom :: Text
    , mailTo :: Text
    , contentTransferEncoding :: Text
    , contentType :: Text
    , mimeVersion :: Text
    , subject :: Text } deriving (Show, Generic)

instance FromJSON ConcertMail
instance ToJSON ConcertMail

application chan request respond = do
    body <- requestBody request
    writeChan chan body
    respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

makeWarpEnv :: IO [(String, String)]
makeWarpEnv = do
    path <- fmap ("PATH",) <$> lookupEnv "PATH"
    home <- fmap ("HOME",) <$> lookupEnv "HOME"
    return $ precanned ++ catMaybes [path, home]
  where
    precanned =
      [ ("SMTP_SERVER", "localhost")
      , ("SMTP_USERNAME", "x")
      , ("SMTP_PASSWORD", "x")
      , ("SMTP_LOGGING", "yes")
      , ("SMTP_PORT", "8025")
      , ("SMTP_DISABLE_SSL_VALIDATION", "yes")-- FIXME: should test with ssl validation

      , ("MAILER_SENDER_ADDRESS", "integration_test@concertdaw.co.uk")
      , ("MAILER_SENDER_NAME", "Concert Integration Test")

      , ("DATABASE_URL", "postgres://postgres:password@localhost:8000/eventful_test")
      , ("PORT", "8080")
      , ("REGISTRATION_UUID_SALT", "NaCl")
      , ("REGISTRATION_USERNAME", "x")
      , ("REGISTRATION_PASSWORD", "x")
      , ("ALLOW_INSECURE", "yes")
      , ("DOMAIN", "localhost:8080")

      , ("COOKIE_KEY", "TAkLv+bu5UpXLGM3Sx4ETXafqjQ1ElexkHV5nHFED3ZOeS0ZLn5VgE25whsAS2ua10Wkp0hAg2Qq42btfgKRiiRpzASlj5gHKfs0tgbxqXtA0R/cfTEtgE1j/tZBGlxV")

      , ("LOGO_URL", "https://example.com/logo.svg")
      , ("LOGO_AND_TEXT_URL", "https://example.com/logo_and_text.svg")
      , ("FAVICON_URL", "https://example.com/favicon.svg")
      , ("DAVE_PROFILE_PIC_URL", "https://example.com/dave_profile_small.jpg")
      , ("PAUL_PROFILE_PIC_URL", "https://example.com/paul_profile_small.jpg")

      , ("COMPANY_ADDRESS", "[\"Number Street\", \"Town\", \"PostCode\", \"Country\"]")
      , ("COMPANY_NUMBER", "10936379")
      ]

main = do
    withCreateProcess (proc "python" ["-m", "aiosmtpmod.server"]) $ \_ _ _ _ -> do
      withDockerD
          ["--rm", "-p", "8000:5432"
          , "-e", "POSTGRES_PASSWORD=password"
          , "-e", "POSTGRES_USER=postgres"
          , "-e", "POSTGRES_DB=eventful_test"
          , "postgres"
          ] $ do
        res <- timeout 5000000 $ retry postgresUp
        when (res == Nothing) exitFailure

        env <- makeWarpEnv
        withCreateProcess (proc "stack" ["exec", "warp-test-exe"]){env = Just env} $
          \_ _ _ _ -> do
            emailChan <- newChan
            app <- async $ run 3000 $ application emailChan

            r <- timeout 5000000 $ retry $ get "http://127.0.0.1:8080/interested" debugHandler
            when (r == Nothing) exitFailure

            hspec $ do
              describe "signUp" $ do
                it "Sends a confirmation email on user sign up" $
                  let
                    useraddr :: IsString s => s
                    useraddr = "test@example.com"
                  in do
                  signUp useraddr
                  -- FIXME: this just hangs if we successfully sign up but
                  -- never get sent an email
                  e <- readChan emailChan
                  let confirmMail = fromJust $ decodeStrict @ConcertMail e
                  (mailTo confirmMail) `shouldBe` "<" <> useraddr <> ">"
            -- TODO:
            --   -check other fields
            --   -follow the link in the mail body to verify
            --     -check the DB
            --     -check the email response
    return ()
