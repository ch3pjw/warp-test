module UuidFor
  ( UuidFor(..)
  , coerceUuidFor
  , toText
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import Database.Persist (PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))

import Eventful.Store.Postgresql () -- For UUID postgresification


newtype UuidFor event = UuidFor {unUuidFor :: UUID} deriving (Eq, Show)

coerceUuidFor :: UuidFor event -> UuidFor event'
coerceUuidFor = UuidFor . unUuidFor

instance ToJSON (UuidFor event) where
  toJSON = toJSON . unUuidFor

instance FromJSON (UuidFor event) where
  parseJSON = fmap UuidFor . parseJSON

instance PersistField (UuidFor a) where
  toPersistValue (UuidFor uuid) = toPersistValue uuid
  fromPersistValue = fmap UuidFor . fromPersistValue

instance PersistFieldSql (UuidFor a) where
  sqlType _ = sqlType (Proxy :: Proxy UUID)

toText :: UuidFor a -> Text
toText = UUID.toText . unUuidFor
