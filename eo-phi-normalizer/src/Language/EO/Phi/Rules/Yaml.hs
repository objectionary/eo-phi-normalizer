{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.EO.Phi.Rules.Yaml where

import Data.Aeson (FromJSON (..))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Language.EO.Phi.Rules.Common as Common
import Language.EO.Phi.Syntax.Abs
import Language.EO.Phi.Syntax.Par

instance IsString Object where
  fromString = unsafeParseObject

instance FromJSON Object where
  parseJSON = fmap fromString . parseJSON

instance FromJSON MetaId where
  parseJSON = fmap MetaId . parseJSON

-- | Parse a 'Object' or return a parsing error.
parseObject :: String -> Either String Object
parseObject input = pObject tokens
 where
  tokens = myLexer input

-- | Parse a 'Object' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseObject :: String -> Object
unsafeParseObject input =
  case parseObject input of
    Left parseError -> error parseError
    Right object -> object

data RuleSet = RuleSet
  { title :: String
  , rules :: [Rule]
  }
  deriving (Generic, FromJSON, Show)

data Rule = Rule
  { name :: String
  , description :: String
  , pattern :: Object
  , result :: Object
  , when :: [Condition]
  , tests :: [RuleTest]
  }
  deriving (Generic, FromJSON, Show)

data RuleTest = RuleTest
  { name :: String
  , input :: Object
  , output :: Object
  , matches :: Bool
  }
  deriving (Generic, FromJSON, Show)

data Condition = IsNF {nf :: [MetaId]}
  deriving (Generic, FromJSON, Show)

parseRuleSetFromFile :: FilePath -> IO RuleSet
parseRuleSetFromFile = Yaml.decodeFileThrow

convertRule :: Rule -> Common.Rule
convertRule Rule{..} _ctx obj =
  [ obj'
  | subst <- matchObject pattern obj
  , obj' <- [applySubst subst result]
  ]

-- input: ⟦ a ↦ ⟦ c ↦ ⟦ ⟧ ⟧, b ↦ ⟦ ⟧ ⟧.a

-- pattern:   ⟦ !a ↦ !n, !B ⟧.!a
-- result:    !n(ρ ↦ ⟦ !B ⟧)

-- match pattern input (get substitution):
--  !a = a
--  !n = ⟦ c ↦ ⟦ ⟧ ⟧
--  !B = b ↦ ⟦ ⟧

-- actual result (after applying substitution):
--  ⟦ c ↦ ⟦ ⟧ ⟧(ρ ↦ ⟦ b ↦ ⟦ ⟧ ⟧)

data Subst = Subst
  { objectMetas :: [(MetaId, Object)]
  , bindingsMetas :: [(MetaId, [Binding])]
  , attributeMetas :: [(MetaId, Attribute)]
  }

instance Semigroup Subst where
  (<>) = mergeSubst

instance Monoid Subst where
  mempty = emptySubst

emptySubst :: Subst
emptySubst = Subst [] [] []

applySubst :: Subst -> Object -> Object
applySubst subst@Subst{..} = \case
  Formation bindings ->
    Formation (applySubstBindings subst bindings)
  Application obj bindings ->
    Application (applySubst subst obj) (applySubstBindings subst bindings)
  ObjectDispatch obj a ->
    ObjectDispatch (applySubst subst obj) (applySubstAttr subst a)
  GlobalDispatch a ->
    GlobalDispatch (applySubstAttr subst a)
  ThisDispatch a ->
    ThisDispatch (applySubstAttr subst a)
  obj@(MetaObject x) -> fromMaybe obj $ lookup x objectMetas
  obj -> obj

applySubstAttr :: Subst -> Attribute -> Attribute
applySubstAttr Subst{..} = \case
  attr@(MetaAttr x) -> fromMaybe attr $ lookup x attributeMetas
  attr -> attr

applySubstBindings :: Subst -> [Binding] -> [Binding]
applySubstBindings subst = concatMap (applySubstBinding subst)

applySubstBinding :: Subst -> Binding -> [Binding]
applySubstBinding subst@Subst{..} = \case
  AlphaBinding a obj ->
    [AlphaBinding (applySubstAttr subst a) (applySubst subst obj)]
  EmptyBinding a ->
    [EmptyBinding (applySubstAttr subst a)]
  DeltaBinding bytes -> [DeltaBinding (coerce bytes)]
  LambdaBinding bytes -> [LambdaBinding (coerce bytes)]
  b@(MetaBindings m) -> fromMaybe [b] (lookup m bindingsMetas)

mergeSubst :: Subst -> Subst -> Subst
mergeSubst (Subst xs ys zs) (Subst xs' ys' zs') =
  Subst (xs ++ xs') (ys ++ ys') (zs ++ zs')

-- 1. need to implement applySubst' :: Subst -> Object -> Object
-- 2. complete the code
matchObject :: Object -> Object -> [Subst]
matchObject (Formation ps) (Formation bs) = matchBindings ps bs
matchObject (Application obj bindings) (Application obj' bindings') = do
  subst1 <- matchObject obj obj'
  subst2 <- matchBindings (applySubstBindings subst1 bindings) bindings'
  pure (subst1 <> subst2)
matchObject (ObjectDispatch pat a) (ObjectDispatch obj a') = do
  subst1 <- matchObject pat obj
  subst2 <- matchAttr (applySubstAttr subst1 a) a'
  pure (subst1 <> subst2)
matchObject (MetaObject m) obj =
  pure
    Subst
      { objectMetas = [(m, obj)]
      , bindingsMetas = []
      , attributeMetas = []
      }
matchObject _ _ = [] -- ? emptySubst ?

matchBindings :: [Binding] -> [Binding] -> [Subst]
matchBindings [] [] = [emptySubst]
matchBindings [MetaBindings b] bindings =
  pure
    Subst
      { objectMetas = []
      , bindingsMetas = [(b, bindings)]
      , attributeMetas = []
      }
matchBindings (p : ps) bs = do
  (bs', subst1) <- matchFindBinding p bs
  subst2 <- matchBindings ps bs'
  pure (subst1 <> subst2)
matchBindings [] _ = []

-- >>> select [1,2,3,4]
-- [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
select :: [a] -> [(a, [a])]
select [] = []
select [x] = [(x, [])]
select (x : xs) =
  (x, xs)
    : [ (y, x : ys)
      | (y, ys) <- select xs
      ]

matchFindBinding :: Binding -> [Binding] -> [([Binding], Subst)]
matchFindBinding p bindings =
  [ (leftover, subst)
  | (binding, leftover) <- select bindings
  , subst <- matchBinding p binding
  ]

matchBinding :: Binding -> Binding -> [Subst]
matchBinding MetaBindings{} _ = []
matchBinding (AlphaBinding a obj) (AlphaBinding a' obj') = do
  subst1 <- matchAttr a a'
  subst2 <- matchObject obj obj'
  pure (subst1 <> subst2)
matchBinding _ _ = []

matchAttr :: Attribute -> Attribute -> [Subst]
matchAttr (MetaAttr metaId@(MetaId name)) attr@(Alpha (AlphaIndex name'))
  | name == name' =
      [ Subst
          { objectMetas = []
          , bindingsMetas = []
          , attributeMetas = [(metaId, attr)]
          }
      ]
matchAttr (MetaAttr metaId@(MetaId name)) attr@(Label (LabelId name'))
  | name == name' =
      [ Subst
          { objectMetas = []
          , bindingsMetas = []
          , attributeMetas = [(metaId, attr)]
          }
      ]
matchAttr _ _ = []
