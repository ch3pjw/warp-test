{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Sessions.ReadViews where

import Control.Monad (void)
import Data.UUID (UUID)
import Data.Text (Text)
import Database.Persist.Postgresql ((=.), (==.))
import qualified Database.Persist.Postgresql as DB
import Database.Persist.TH (
    share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import Eventful.Store.Postgresql () -- For UUID postgresification

import Events
  ( EmailAddress, UserAgentString(..), Event, SessionEvent(..), TimeStamped,
  decomposeEvent, UuidFor(..))
import ReadView (ReadView, simpleReadView, liftReadView)


share [mkPersist sqlSettings, mkMigrate "migrateAS"] [persistLowerCase|
ActiveSession
    sessionUuid UUID
    accountUuid UUID Maybe
    userAgent Text Maybe
    emailAddress EmailAddress
    UniqueUuid sessionUuid
    deriving Show
|]

-- FIXME: could make the DB model here directly refer to (UuidFor SessionEvent)
-- and (UuidFor AccountEvent), rather than just UUID.

_activeSessionsReadView :: ReadView SessionEvent
_activeSessionsReadView = simpleReadView  "active_sessions" migrateAS update
  where
    update uuid (SessionRequestedSessionEvent e) =
        void $ DB.insertBy $ ActiveSession uuid Nothing Nothing e
    update sUuid (SessionAssociatedWithAccountSessionEvent aUuid) =
        DB.updateWhere
          [ActiveSessionSessionUuid ==. sUuid]
          [ActiveSessionAccountUuid =. Just (unUuidFor aUuid)]
    update uuid (SessionSignedInSessionEvent uaString) =
        DB.updateWhere
          [ActiveSessionSessionUuid ==. uuid]
          [ActiveSessionUserAgent =. Just (unUserAgentString uaString)]
    update uuid SessionSignedOutSessionEvent = DB.deleteBy $ UniqueUuid uuid
    update _ SessionSignInEmailSentSessionEvent = return ()

activeSessionReadView :: ReadView (TimeStamped Event)
activeSessionReadView =
    liftReadView (decomposeEvent ignore ignore Just . snd)
    _activeSessionsReadView
  where
    ignore = const Nothing
