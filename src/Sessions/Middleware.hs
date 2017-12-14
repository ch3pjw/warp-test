module Sessions.Middleware where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool (Pool)
import qualified Database.Persist.Postgresql as DB
import qualified Network.Wai.Trans as Wai
import Web.ClientSession (Key)

import Sessions.Cookies (SessionCookie, extractSessionCookie)
import Sessions.ReadViews (validateSessionCookie)


maybeWithSessionCookie
  :: (Monad m)
  => Key -> Wai.ApplicationT m -> (SessionCookie -> Wai.ApplicationT m)
  -> Wai.ApplicationT m
maybeWithSessionCookie key defApp withScApp req sendResponse =
    either
      (const defApp)
      withScApp
      (extractSessionCookie key req)
    req sendResponse

maybeWithValidatedSessionCookie
  :: (MonadIO m)
  => Key -> Pool DB.SqlBackend -> Wai.ApplicationT m
  -> (SessionCookie -> Wai.ApplicationT m) -> Wai.ApplicationT m
maybeWithValidatedSessionCookie key pool defApp withScApp =
    maybeWithSessionCookie key defApp withValidatedScApp
  where
    withValidatedScApp sc req sendResponse = do
      msc' <- liftIO $ DB.runSqlPool (validateSessionCookie sc) pool
      (maybe defApp withScApp msc') req sendResponse
