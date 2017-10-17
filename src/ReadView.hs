{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ReadView
  ( DB.EntityField(..)
  , EmailRegistrationId
  , emailRegistrationEmailAddress
  , ViewSequenceNumberId
  , viewWorker
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Persist.Postgresql ((=.), (==.))
import qualified Database.Persist.Postgresql as DB
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Eventful (
    SequenceNumber(..), GlobalStreamEvent,
    getEvents, eventsStartingAt, streamEventEvent, streamEventKey,
    streamEventPosition)
import Eventful.Store.Postgresql (serializedGlobalEventStoreReader)
import Eventful.Store.Sql (jsonStringSerializer, defaultSqlEventStoreConfig, sqlGlobalEventStoreReader)

import Registration (EmailAddress, UserEvent(..), TimeStamped, untilNothing)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
EmailRegistration
    uuid UUID
    emailAddress EmailAddress
    verified Bool
    UniqueEmailAddress emailAddress
    UniqueUuid uuid
    deriving Show

ViewSequenceNumber
    name Text
    latestApplied SequenceNumber
    UniqueName name
    deriving Show
|]

-- FIXME: this constant needs to line up with what the template stuff above
-- produces, and is _not_ checked :-/
erTableName :: Text
erTableName = "email_registration"


latestEvents
  :: (MonadIO m)
  => SequenceNumber
  -> DB.SqlPersistT m [GlobalStreamEvent (TimeStamped UserEvent)]
latestEvents latestHandled =
    getEvents eventReader (eventsStartingAt () $ latestHandled + 1)
  where
    jsonReader = sqlGlobalEventStoreReader defaultSqlEventStoreConfig
    eventReader = serializedGlobalEventStoreReader jsonStringSerializer jsonReader


handleUserStateReadModelEvents
  :: [GlobalStreamEvent (TimeStamped UserEvent)] -> ReaderT DB.SqlBackend IO ()
handleUserStateReadModelEvents events = do
    mapM_ (uncurry mutate . decomposeEvent) events
    updateSN events
  where
    mutate uuid (_, UserSubmitted email) = void $ DB.insertBy $
        EmailRegistration uuid email False
    mutate uuid (_, UserVerified) = DB.updateWhere
        [EmailRegistrationUuid ==. uuid]
        [EmailRegistrationVerified =. True]
    mutate uuid (_, UserUnsubscribed) = DB.deleteBy $ UniqueUuid uuid
    mutate _ _ = return ()
    decomposeEvent e = let e' = streamEventEvent e in
        (streamEventKey e', streamEventEvent e')
    updateSN [] = return ()
    updateSN es = let latestSN = streamEventPosition $ last es in
        DB.updateWhere
          [ViewSequenceNumberName ==. erTableName]
          [ViewSequenceNumberLatestApplied =. latestSN]


initialiseUserStateView :: ReaderT DB.SqlBackend IO ()
initialiseUserStateView = DB.runMigration migrateAll >> void (
    getOrInitSequenceNumber erTableName)

getOrInitSequenceNumber :: Text -> ReaderT DB.SqlBackend IO SequenceNumber
getOrInitSequenceNumber tableName = do
    eVsn <- DB.insertBy $ ViewSequenceNumber tableName 0
    return $ either (getN . DB.entityVal) (const 0) eVsn
  where
    getN (ViewSequenceNumber _ n) = n


updateUserStateView :: ReaderT DB.SqlBackend IO ()
updateUserStateView =
      getOrInitSequenceNumber erTableName >>=
      latestEvents >>= handleUserStateReadModelEvents


viewWorker :: Pool DB.SqlBackend -> (IO (Maybe a)) -> IO ()
viewWorker pool wait = DB.runSqlPool i pool >> f
  where
    f = untilNothing wait (const $ DB.runSqlPool updateUserStateView pool)
    i = DB.runMigration migrateAll >> initialiseUserStateView
        >> updateUserStateView
