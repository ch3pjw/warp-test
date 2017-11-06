{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Error.Util
import Control.Monad (join)
import Data.Text (pack, unpack)
import qualified Data.Text as Text

import Language.Haskell.TH

mashSumTypes :: String -> [Name] -> Q [Dec]
mashSumTypes _ [] = return []
mashSumTypes newNameStr typeNames =
  do
    newConstructors <- join <$> mapM mkNewConstructors typeNames
    return [DataD [] (mkName newNameStr) [] Nothing newConstructors []]
  where
    mkNewConstructors typeName = do
      info <- reify typeName
      case info of
        (TyConI (DataD _ _ _ _ constructors _)) ->
          either fail return $ mapM (modConstructor typeName) constructors
        _ -> fail $ nameBase typeName ++ " must be a sum type"

    modConstructor :: Name -> Con -> Either String Con
    modConstructor typeName (NormalC conName args) =
        (\n -> (NormalC (mkName $ n ++ newNameStr) args)) <$>
        note "Constructor name missing type name suffix"
          (stripNameSuffix typeName conName)
    modConstructor _ _ = fail "Unrecognised constructor pattern"

    stripNameSuffix :: Name -> Name -> Maybe String
    stripNameSuffix suf = stripSuffix (nameBase suf) . nameBase


stripSuffix :: String -> String -> Maybe String
stripSuffix suf = fmap unpack . Text.stripSuffix (pack suf) . pack
