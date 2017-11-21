{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Store.Db where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson (ToJSON, FromJSON)
import Data.Pool (Pool)
import Data.UUID (UUID)
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import qualified Database.Persist.Postgresql as DB
import Eventful
  ( SequenceNumber
  , EventVersion
  , serializedEventStoreWriter
  , serializedVersionedEventStoreReader
  )
import Eventful.Store.Sql
  ( SqlEventStoreConfig(..)
  , JSONString
  , jsonStringSerializer
  , sqlEventStoreReader
  )
import Eventful.Store.Postgresql
  ( postgresqlEventStoreWriter
  )

import Store.Types
  (Store
  , newStoreFrom
  , liftEventStoreReader
  , liftEventStoreWriter
  )


share [mkPersist sqlSettings, mkMigrate "migrateSE2"] [persistLowerCase|
SqlEvent2 sql=events_2
    Id SequenceNumber sql=sequence_number
    uuid UUID
    version EventVersion
    event JSONString
    UniqueUuidVersion2 uuid version
    deriving Show
|]


eventStoreConfig :: SqlEventStoreConfig SqlEvent2 JSONString
eventStoreConfig =
  SqlEventStoreConfig
  SqlEvent2
  SqlEvent2Key
  (\(SqlEvent2Key seqNum) -> seqNum)
  sqlEvent2Uuid
  sqlEvent2Version
  sqlEvent2Event
  SqlEvent2Id
  SqlEvent2Uuid
  SqlEvent2Version
  SqlEvent2Event

newDBStore
  :: (MonadIO m, MonadBaseControl IO m, ToJSON event, FromJSON event)
  => Pool DB.SqlBackend -> IO (Store m event)
newDBStore pool =
  let
    run :: (MonadBaseControl IO m) => ReaderT DB.SqlBackend m a -> m a
    run = flip DB.runSqlPool pool
    writer = liftEventStoreWriter run $
        serializedEventStoreWriter jsonStringSerializer $
        postgresqlEventStoreWriter eventStoreConfig
    reader = liftEventStoreReader run $
        serializedVersionedEventStoreReader jsonStringSerializer $
        sqlEventStoreReader eventStoreConfig
  in do
    DB.runSqlPool (DB.runMigration migrateSE2) pool
    newStoreFrom writer reader
