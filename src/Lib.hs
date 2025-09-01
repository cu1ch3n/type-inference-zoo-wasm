{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib (InferMonad, runInferMonad, freshTVar, freshLVar, break3, Derivation (..), InferResult (..), inferResultToJson, Paper (..), Variant (..), Rule (..), RuleGroup (..), Example (..), AlgMeta (..), algMetasToJson) where

import Control.Monad.RWS (MonadTrans (lift), RWST, get, put, runRWST)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (ToJSON (..), defaultOptions, encode, fieldLabelModifier, genericToJSON, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as L8
import GHC.Generics (Generic)
import Syntax (LabelVar, TyVar)
import Unbound.Generics.LocallyNameless (FreshMT, runFreshMT)
import Unbound.Generics.LocallyNameless.Fresh (Fresh (..))
import Unbound.Generics.LocallyNameless.Name (s2n)

type InferMonad = ExceptT String (RWST () [String] Int (FreshMT Maybe))

instance (Monoid w, Fresh m) => Fresh (RWST r w s m) where
  fresh = lift . fresh

runInferMonad :: InferMonad a -> Either [String] (a, [String])
runInferMonad m = case runFreshMT $ runRWST (runExceptT m) () 0 of
  Nothing -> Left ["Computation failed"]
  Just (Left s, _, msgs) -> Left (s : msgs)
  Just (Right res, _, msgs) -> Right (res, msgs)

freshTVar :: InferMonad TyVar
freshTVar = do
  let letters = ["a", "b", "c", "d"]
  varId <- get
  put (varId + 1)
  fresh . s2n $ letters !! (varId `mod` length letters)

freshLVar :: InferMonad LabelVar
freshLVar = do
  let letters = ["l", "m", "n", "o"]
  varId <- get
  put (varId + 1)
  fresh . s2n $ letters !! (varId `mod` length letters)

break3 :: (a -> Bool) -> [a] -> ([a], Maybe a, [a])
break3 p xs = case break p xs of
  (ys, []) -> (ys, Nothing, [])
  (ys, z : zs) -> (ys, Just z, zs)

data Derivation = Derivation
  { ruleId :: String,
    expression :: String,
    children :: [Derivation]
  }

data InferResult = InferResult
  { success :: Bool,
    finalType :: Maybe String,
    derivation :: [Derivation],
    errorMsg :: Maybe String,
    errorLatex :: Bool
  }

instance ToJSON Derivation where
  toJSON step =
    object
      [ Key.fromString "ruleId" .= ruleId step,
        Key.fromString "expression" .= expression step,
        Key.fromString "children" .= children step
      ]

instance ToJSON InferResult where
  toJSON result =
    object
      [ Key.fromString "success" .= success result,
        Key.fromString "finalType" .= finalType result,
        Key.fromString "derivation" .= derivation result,
        Key.fromString "error" .= errorMsg result,
        Key.fromString "errorLatex" .= errorLatex result
      ]

inferResultToJson :: InferResult -> String
inferResultToJson = L8.unpack . encode

-- | Paper metadata
data Paper = Paper
  { paperTitle :: String,
    paperAuthors :: [String],
    paperYear :: Int,
    paperUrl :: String
  }
  deriving (Generic, Show)

-- | Algorithm variant metadata
data Variant = Variant
  { variantId :: String,
    variantName :: String,
    variantDescription :: String
  }
  deriving (Generic, Show)

-- | Inference rule metadata
data Rule = Rule
  { metaRuleId :: String, -- Renamed to avoid conflict with Derivation.ruleId
    metaRuleName :: String,
    metaRulePremises :: [String],
    metaRuleConclusion :: String,
    metaRuleDescription :: Maybe String,
    metaRuleReduction :: Maybe String -- For worklist-style rules
  }
  deriving (Generic, Show)

-- | Rule group for contextual-style algorithms
data RuleGroup = RuleGroup
  { groupId :: String,
    groupName :: String,
    groupDescription :: Maybe String,
    groupFormula :: Maybe String,
    groupRules :: [Rule]
  }
  deriving (Generic, Show)

-- | Algorithm metadata
data AlgMeta = AlgMeta
  { metaId :: String,
    metaName :: String,
    metaLabels :: [String],
    metaViewMode :: String, -- "tree" or "linear"
    metaMode :: String, -- "inference" or "subtyping"
    metaPaper :: Paper,
    metaVariants :: Maybe [Variant],
    metaDefaultVariant :: Maybe String,
    metaRules :: [Rule],
    metaRuleGroups :: Maybe [RuleGroup], -- For contextual-style rules
    metaVariantRules :: Maybe [(String, [RuleGroup])], -- For variant-specific rules
    metaExamples :: [Example] -- Algorithm examples
  }
  deriving (Generic, Show)

-- | Example metadata
data Example = Example
  { exampleName :: String,
    exampleExpression :: String,
    exampleDescription :: String
  }
  deriving (Generic, Show)

-- Custom ToJSON instances to match the frontend format
instance ToJSON Paper where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 5} -- Remove "paper" prefix

instance ToJSON Variant where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 7} -- Remove "variant" prefix

instance ToJSON Rule where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \s -> case s of
            "metaRuleId" -> "Id"
            "metaRuleName" -> "Name"
            "metaRulePremises" -> "Premises"
            "metaRuleConclusion" -> "Conclusion"
            "metaRuleDescription" -> "Description"
            "metaRuleReduction" -> "Reduction"
            _ -> drop 4 s -- Remove "meta" prefix for other fields
        }

instance ToJSON Example where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 7} -- Remove "example" prefix

instance ToJSON RuleGroup where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 5} -- Remove "group" prefix

instance ToJSON AlgMeta where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 4} -- Remove "meta" prefix

-- | Collect all algorithm metadata
algMetasToJson :: [AlgMeta] -> String
algMetasToJson = L8.unpack . encode
