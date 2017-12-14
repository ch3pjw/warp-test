{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Registration.ReadViews
  ( DB.EntityField(..)
  , emailRegistrationEmailAddress
  , mkEmailStateReadView
  ) where

import Control.Monad (void)
import Data.Pool (Pool)
import Data.UUID (UUID)
import Database.Persist.Postgresql ((=.), (==.))
import qualified Database.Persist.Postgresql as DB
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import Events
  ( EmailAddress
  , Event( EmailAddressSubmittedEvent
         , EmailAddressVerifiedEvent
         , EmailAddressRemovedEvent
         )
  , TimeStamped
  )
import ReadView (ReadView, simpleReadView)
import UuidFor (unUuidFor)

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
mkEmailStateReadView :: Pool DB.SqlBackend -> ReadView (TimeStamped Event)
mkEmailStateReadView pool =
    simpleReadView  "email_registration" migrateER
       (\uuid' event -> DB.runSqlPool (update uuid' event) pool)
  where
    update uuid' (_, EmailAddressSubmittedEvent email) = void $ DB.insertBy $
        EmailRegistration (unUuidFor uuid') email False
    update uuid' (_, EmailAddressVerifiedEvent) = DB.updateWhere
        [EmailRegistrationUuid ==. unUuidFor uuid']
        [EmailRegistrationVerified =. True]
    update uuid' (_, EmailAddressRemovedEvent) =
        DB.deleteBy $ UniqueUuid $ unUuidFor uuid'
    update _ _ = return ()
