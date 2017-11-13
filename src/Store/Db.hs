{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Store.Db where

import Control.Monad (void)
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
  , getLatestStreamProjection
  , versionedStreamProjection
  , storeEvents
  )
import Eventful.Store.Memory
  ( ExpectedPosition(AnyPosition)
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

import Store.Types (Store, newStoreFrom)


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
  :: (ToJSON event, FromJSON event)
  => Pool DB.SqlBackend -> IO (Store event)
newDBStore pool =
  let
    writer = serializedEventStoreWriter jsonStringSerializer $
        postgresqlEventStoreWriter eventStoreConfig
    reader = serializedVersionedEventStoreReader jsonStringSerializer $
        sqlEventStoreReader eventStoreConfig
  in do
    DB.runSqlPool (DB.runMigration migrateSE2) pool
    newStoreFrom
      (\uuid events -> void $
          DB.runSqlPool (storeEvents writer uuid AnyPosition events) pool)
      (\initialProjection uuid -> DB.runSqlPool
          (getLatestStreamProjection reader $
              versionedStreamProjection uuid initialProjection) pool)
