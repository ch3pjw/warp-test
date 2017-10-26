{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Data.Char (toLower)
import Data.Stringable (Stringable, toString)
import Network.URI (URI, parseURI)
import System.Envy (Var, fromVar, toVar)


newtype Password a = Password { unPassword :: a } deriving (Eq)

instance Show (Password a) where
    show _ = "******"

instance (Var a, Stringable a) => Var (Password a) where
    fromVar s = Password <$> fromVar s
    toVar = toString . unPassword


newtype EnvToggle = EnvToggle { unEnvToggle :: Bool } deriving (Eq, Show)

instance Var EnvToggle where
    fromVar s' = let s = toLower <$> s' in
        return . EnvToggle . not . any (s ==) $ ["", "f", "false", "n", "no"]
    toVar = show . unEnvToggle


instance Var URI where
    fromVar = parseURI
    toVar = show
