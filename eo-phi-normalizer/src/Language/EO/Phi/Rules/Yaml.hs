{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.EO.Phi.Rules.Yaml where

import Data.Aeson (FromJSON (..))
import Data.Coerce (coerce)
import Data.String (IsString (..))
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Language.EO.Phi.Rules.Common as Common
import qualified Language.EO.Phi.Syntax.Abs as Phi
import qualified Language.EO.Phi.Syntax.Par as Phi

instance IsString Phi.Object where
  fromString = unsafeParseObject

instance FromJSON Phi.Object where
  parseJSON = fmap fromString . parseJSON

instance FromJSON Phi.MetaId where
  parseJSON = fmap Phi.MetaId . parseJSON

-- | Parse a 'Object' or return a parsing error.
parseObject :: String -> Either String Phi.Object
parseObject input = Phi.pObject tokens
 where
  tokens = Phi.myLexer input

-- | Parse a 'Object' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseObject :: String -> Phi.Object
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
  , pattern :: Phi.Object
  , result :: Phi.Object
  , when :: [Condition]
  }
  deriving (Generic, FromJSON, Show)

data Condition = IsNF {nf :: [Phi.MetaId]}
  deriving (Generic, FromJSON, Show)

parseRuleSetFromFile :: FilePath -> IO RuleSet
parseRuleSetFromFile = Yaml.decodeFileThrow

convertRule :: Rule -> Common.Rule
convertRule Rule{..} _ctx obj =
  [ obj'
  | subst <- matchObject pattern obj
  , Just obj' <- [applySubst subst result]
  ]

-- input: ⟦ a ↦ ⟦ c ↦ ⟦ ⟧ ⟧, b ↦ ⟦ ⟧ ⟧.a

-- pattern:   ⟦ ?a ↦ ?n, ?B ⟧.?a
-- result:    ?n(ρ ↦ ⟦ ?B ⟧)

-- match pattern input (get substitution):
--  ?a = a
--  ?n = ⟦ c ↦ ⟦ ⟧ ⟧
--  ?B = b ↦ ⟦ ⟧

-- actual result (after applying substitution):
--  ⟦ c ↦ ⟦ ⟧ ⟧(ρ ↦ ⟦ b ↦ ⟦ ⟧ ⟧)

data Subst = Subst
  { objectMetas :: [(Phi.MetaId, Phi.Object)]
  , bindingsMetas :: [(Phi.MetaId, [Phi.Binding])]
  , attributeMetas :: [(Phi.MetaId, Phi.Attribute)]
  }

instance Semigroup Subst where
  (<>) = mergeSubst

instance Monoid Subst where
  mempty = emptySubst

emptySubst :: Subst
emptySubst = Subst [] [] []

applySubst :: Subst -> Phi.Object -> Maybe Phi.Object
applySubst subst@Subst{..} = \case
  Phi.Formation bindings -> do
    bindings' <- applySubstBindings subst bindings
    return (Phi.Formation bindings')
  Phi.Application obj bindings -> do
    obj' <- applySubst subst obj
    bindings' <- applySubstBindings subst bindings
    return (Phi.Application obj' bindings')
  Phi.ObjectDispatch obj a -> do
    obj' <- applySubst subst obj
    a' <- applySubstAttr subst a
    return (Phi.ObjectDispatch obj' a')
  Phi.GlobalDispatch a -> do
    a' <- applySubstAttr subst a
    return (Phi.GlobalDispatch a')
  Phi.ThisDispatch a -> do
    a' <- applySubstAttr subst a
    return (Phi.ThisDispatch a')
  Phi.Termination -> return Phi.Termination
  Phi.MetaObject x -> lookup x objectMetas

applySubstAttr :: Subst -> Phi.Attribute -> Maybe Phi.Attribute
applySubstAttr Subst{..} = \case
  Phi.Phi -> return Phi.Phi
  Phi.Rho -> return Phi.Rho
  Phi.Sigma -> return Phi.Sigma
  Phi.VTX -> return Phi.VTX
  Phi.Label l -> return (Phi.Label (coerce l))
  Phi.Alpha a -> return (Phi.Alpha (coerce a))
  Phi.MetaAttr x -> lookup x attributeMetas

applySubstBindings :: Subst -> [Phi.Binding] -> Maybe [Phi.Binding]
applySubstBindings subst bindings =
  concat <$> mapM (applySubstBinding subst) bindings

applySubstBinding :: Subst -> Phi.Binding -> Maybe [Phi.Binding]
applySubstBinding subst@Subst{..} = \case
  Phi.AlphaBinding a obj -> do
    a' <- applySubstAttr subst a
    obj' <- applySubst subst obj
    return [Phi.AlphaBinding a' obj']
  Phi.EmptyBinding a -> do
    a' <- applySubstAttr subst a
    return [Phi.EmptyBinding a']
  Phi.DeltaBinding bytes -> return [Phi.DeltaBinding (coerce bytes)]
  Phi.LambdaBinding bytes -> return [Phi.LambdaBinding (coerce bytes)]
  Phi.MetaBindings m -> lookup m bindingsMetas

mergeSubst :: Subst -> Subst -> Subst
mergeSubst (Subst xs ys zs) (Subst xs' ys' zs') =
  Subst (xs ++ xs') (ys ++ ys') (zs ++ zs')

-- 1. need to implement applySubst' :: Subst -> Phi.Object -> Phi.Object
-- 2. complete the code
matchObject :: Phi.Object -> Phi.Object -> [Subst]
matchObject (Phi.Formation ps) (Phi.Formation bs) =
  matchBindings ps bs
matchObject (Phi.Application obj bindings) (Phi.Application obj' bindings') = do
  subst1 <- matchObject obj obj'
  subst2 <- matchBindings (applySubstBindings' subst1 bindings) bindings'
  return (subst1 <> subst2)
matchObject (Phi.ObjectDispatch pat a) (Phi.ObjectDispatch obj a') = do
  subst1 <- matchObject pat obj
  subst2 <- matchAttr (applySubstAttr' subst1 a) a'
  return (subst1 <> subst2)
matchObject (Phi.MetaObject m) obj =
  return
    Subst
      { objectMetas = [(m, obj)]
      , bindingsMetas = []
      , attributeMetas = []
      }

applySubstBindings' = error "TODO"
applySubstAttr' = error "TODO"

matchBindings :: [Phi.Binding] -> [Phi.Binding] -> [Subst]
matchBindings [] [] = [emptySubst]
matchBindings [Phi.MetaBindings b] bindings =
  return
    Subst
      { objectMetas = []
      , bindingsMetas = [(b, bindings)]
      , attributeMetas = []
      }
matchBindings (p : ps) bs = do
  (bs', subst1) <- matchFindBinding p bs
  subst2 <- matchBindings ps bs'
  return (subst1 <> subst2)

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

matchFindBinding :: Phi.Binding -> [Phi.Binding] -> [([Phi.Binding], Subst)]
matchFindBinding p bindings =
  [ (leftover, subst)
  | (binding, leftover) <- select bindings
  , subst <- matchBinding p binding
  ]

matchBinding :: Phi.Binding -> Phi.Binding -> [Subst]
matchBinding Phi.MetaBindings{} _ = []
matchBinding (Phi.AlphaBinding a obj) (Phi.AlphaBinding a' obj') = do
  subst1 <- matchAttr a a'
  subst2 <- matchObject obj obj'
  return (subst1 <> subst2)

matchAttr :: Phi.Attribute -> Phi.Attribute -> [Subst]
matchAttr _ _ = []
