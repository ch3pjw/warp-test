{-# LANGUAGE OverloadedStrings #-}

module Accounts.Model where

import Data.Text (Text)

import Eventful (Projection(..))

import Events (AccountEvent(..))

data AccountState
  = AccountState
  { asAccountName :: Text
  , asServiceEmailsEnabled :: Bool
  } deriving (Eq, Show)

initialAccountState :: AccountState
initialAccountState = AccountState "" True

updateAccountState :: AccountState -> AccountEvent -> AccountState
updateAccountState s (AccountNameUpdatedAccountEvent name) =
  s {asAccountName = name}
updateAccountState s ServiceUpdateEmailsEnabledAccountEvent =
  s {asServiceEmailsEnabled = True}
updateAccountState s ServiceUpdateEmailsDisabledAccountEvent =
  s {asServiceEmailsEnabled = False}
updateAccountState _ _ = initialAccountState

initialAccountProjection :: Projection AccountState AccountEvent
initialAccountProjection = Projection initialAccountState updateAccountState
