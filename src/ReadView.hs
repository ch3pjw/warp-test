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
  , userStateReadView
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Data.Aeson (ToJSON, FromJSON)
import Data.Monoid
import Data.Pool (Pool)
import Data.Text (Text, unpack)
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


share [mkPersist sqlSettings, mkMigrate "migrateVSN"] [persistLowerCase|
ViewSequenceNumber
    name Text
    latestApplied SequenceNumber
    UniqueName name
    deriving Show
|]

data ReadView event = ReadView
  { rvTableName :: Text
  , rvMigration :: DB.Migration
  , rvUpdate :: UUID -> event -> ReaderT DB.SqlBackend IO ()
  }

instance Show (ReadView event) where
    show rv = unpack $ "<ReadView " <> (rvTableName rv) <> ">"


latestEvents
  :: (MonadIO m, ToJSON event, FromJSON event)
  => SequenceNumber
  -> DB.SqlPersistT m [GlobalStreamEvent event]
latestEvents latestHandled =
    getEvents eventReader (eventsStartingAt () $ latestHandled + 1)
  where
    jsonReader = sqlGlobalEventStoreReader defaultSqlEventStoreConfig
    eventReader = serializedGlobalEventStoreReader jsonStringSerializer jsonReader


handleReadViewEvents
  :: ReadView event -> [GlobalStreamEvent event] -> ReaderT DB.SqlBackend IO ()
handleReadViewEvents rv events = do
    mapM_ (uncurry (rvUpdate rv) . decomposeEvent) events
    updateSN events
  where
    decomposeEvent e = let e' = streamEventEvent e in
        (streamEventKey e', streamEventEvent e')
    updateSN [] = return ()
    updateSN es = let latestSN = streamEventPosition $ last es in
        DB.updateWhere
          [ViewSequenceNumberName ==. rvTableName rv]
          [ViewSequenceNumberLatestApplied =. latestSN]


initialiseReadView :: ReadView event -> ReaderT DB.SqlBackend IO ()
initialiseReadView rv = do
    DB.runMigration migrateVSN  -- Presumably OK to do evey time?
    DB.runMigration $ rvMigration rv
    void . getOrInitSequenceNumber $ rvTableName rv

getOrInitSequenceNumber :: Text -> ReaderT DB.SqlBackend IO SequenceNumber
getOrInitSequenceNumber tableName = do
    eVsn <- DB.insertBy $ ViewSequenceNumber tableName 0
    return $ either (getN . DB.entityVal) (const 0) eVsn
  where
    getN (ViewSequenceNumber _ n) = n


updateReadView
  :: (ToJSON event, FromJSON event)
  => ReadView event -> ReaderT DB.SqlBackend IO ()
updateReadView rv =
      getOrInitSequenceNumber (rvTableName rv) >>=
      latestEvents >>= handleReadViewEvents rv


viewWorker
  :: (ToJSON event, FromJSON event)
  => ReadView event -> Pool DB.SqlBackend -> (IO (Maybe a)) -> IO ()
viewWorker rv pool wait = DB.runSqlPool i pool >> f
  where
    f = untilNothing wait (const $ DB.runSqlPool (updateReadView rv) pool)
    i = initialiseReadView rv >> updateReadView rv


share [mkPersist sqlSettings, mkMigrate "migrateER"] [persistLowerCase|
EmailRegistration
    uuid UUID
    emailAddress EmailAddress
    verified Bool
    UniqueEmailAddress emailAddress
    UniqueUuid uuid
    deriving Show
|]

-- FIXME: table name needs to line up with what the template stuff above
-- produces, and is _not_ checked :-/
userStateReadView :: ReadView (TimeStamped UserEvent)
userStateReadView = ReadView  "email_registration" migrateER update
  where
    update uuid (_, UserSubmitted email) = void $ DB.insertBy $
        EmailRegistration uuid email False
    update uuid (_, UserVerified) = DB.updateWhere
        [EmailRegistrationUuid ==. uuid]
        [EmailRegistrationVerified =. True]
    update uuid (_, UserUnsubscribed) = DB.deleteBy $ UniqueUuid uuid
    update _ _ = return ()
