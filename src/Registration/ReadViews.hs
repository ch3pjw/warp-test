{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Registration.ReadViews where

import Data.UUID (UUID)
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import Events (EmailAddress)
import ReadView (simpleReadView)

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
userStateReadView :: ReadView (TimeStamped Event)
userStateReadView = simpleReadView  "email_registration" migrateER update
  where
    update uuid (_, EmailAddressSubmittedEvent email) = void $ DB.insertBy $
        EmailRegistration uuid email False
    update uuid (_, EmailAddressVerifiedEvent) = DB.updateWhere
        [EmailRegistrationUuid ==. uuid]
        [EmailRegistrationVerified =. True]
    update uuid (_, EmailAddressRemovedEvent) = DB.deleteBy $ UniqueUuid uuid
    update _ _ = return ()
