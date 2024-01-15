{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.EO.Phi.Rules.Yaml where

import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import qualified Data.Yaml as Yaml
import Data.String (IsString(..))
import qualified Language.EO.Phi.Rules.Syntax.Abs as Rules
import qualified Language.EO.Phi.Rules.Syntax.Par as Rules
import qualified Language.EO.Phi.Syntax.Abs as Phi

import qualified Language.EO.Phi.Rules.Common as Common

instance IsString Rules.Object where
  fromString = unsafeParseObject

instance FromJSON Rules.Object where
  parseJSON = fmap fromString . parseJSON

instance FromJSON Rules.MetaId where
  parseJSON = fmap Rules.MetaId . parseJSON

-- | Parse a 'Object' or return a parsing error.
parseObject :: String -> Either String Rules.Object
parseObject input = Rules.pObject tokens
 where
  tokens = Rules.myLexer input

-- | Parse a 'Object' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseObject :: String -> Rules.Object
unsafeParseObject input =
  case parseObject input of
    Left parseError -> error parseError
    Right object -> object


data RuleSet = RuleSet
  { title :: String
  , rules :: [Rule]
  } deriving (Generic, FromJSON, Show)

data Rule = Rule
  { name :: String
  , description :: String
  , pattern :: Rules.Object
  , result :: Rules.Object
  , when :: [Condition]
  } deriving (Generic, FromJSON, Show)

data Condition
  = IsNF { nf :: [Rules.MetaId] }
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
  { objectMetas     :: [(Rules.MetaId, Phi.Object)]
  , bindingsMetas   :: [(Rules.MetaId, [Phi.Binding])]
  , attributeMetas  :: [(Rules.MetaId, Phi.Attribute)]
  }

instance Semigroup Subst where
  (<>) = mergeSubst

instance Monoid Subst where
  mempty = emptySubst

emptySubst :: Subst
emptySubst = Subst [] [] []

applySubst :: Subst -> Rules.Object -> Maybe (Phi.Object)
applySubst subst@Subst{..} = \case
  Rules.Formation bindings -> do
    bindings' <- applySubstBindings subst bindings
    return (Phi.Formation bindings')

  Rules.Application obj bindings -> do
    obj' <- applySubst subst obj
    bindings' <- applySubstBindings subst bindings
    return (Phi.Application obj' bindings')

  Rules.ObjectDispatch obj a -> do
    obj' <- applySubst subst obj
    a' <- applySubstAttr subst a
    return (Phi.ObjectDispatch obj' a')

  Rules.GlobalDispatch a -> do
    a' <- applySubstAttr subst a
    return (Phi.GlobalDispatch a')

  Rules.ThisDispatch a -> do
    a' <- applySubstAttr subst a
    return (Phi.ThisDispatch a')

  Rules.Termination -> return Phi.Termination

  Rules.MetaObject x -> lookup x objectMetas

applySubstAttr :: Subst -> Rules.Attribute -> Maybe (Phi.Attribute)
applySubstAttr Subst{..} = \case
  Rules.Phi     -> return Phi.Phi
  Rules.Rho     -> return Phi.Rho
  Rules.Sigma   -> return Phi.Sigma
  Rules.VTX     -> return Phi.VTX
  Rules.Label l -> return (Phi.Label (coerce l))
  Rules.Alpha a -> return (Phi.Alpha (coerce a))
  Rules.MetaAttr x -> lookup x attributeMetas

applySubstBindings :: Subst -> [Rules.Binding] -> Maybe [Phi.Binding]
applySubstBindings subst bindings =
  concat <$> mapM (applySubstBinding subst) bindings

applySubstBinding :: Subst -> Rules.Binding -> Maybe [Phi.Binding]
applySubstBinding subst@Subst{..} = \case
  Rules.AlphaBinding a obj -> do
    a' <- applySubstAttr subst a
    obj' <- applySubst subst obj
    return [Phi.AlphaBinding a' obj']
  Rules.EmptyBinding a -> do
    a' <- applySubstAttr subst a
    return [Phi.EmptyBinding a']
  Rules.DeltaBinding bytes -> return [Phi.DeltaBinding (coerce bytes)]
  Rules.LambdaBinding bytes -> return [Phi.LambdaBinding (coerce bytes)]
  Rules.MetaBindings m -> lookup m bindingsMetas

mergeSubst :: Subst -> Subst -> Subst
mergeSubst (Subst xs ys zs) (Subst xs' ys' zs') =
  Subst (xs ++ xs') (ys ++ ys') (zs ++ zs')

-- 1. need to implement applySubst' :: Subst -> Rules.Object -> Rules.Object
-- 2. complete the code
matchObject :: Rules.Object -> Phi.Object -> [Subst]
matchObject (Rules.Formation ps) (Phi.Formation bs) =
  matchBindings ps bs
matchObject (Rules.Application obj bindings) (Phi.Application obj' bindings') = do
  subst1 <- matchObject obj obj'
  subst2 <- matchBindings (applySubstBindings' subst1 bindings) bindings'
  return (subst1 <> subst2)
matchObject (Rules.ObjectDispatch pat a) (Phi.ObjectDispatch obj a') = do
  subst1 <- matchObject pat obj
  subst2 <- matchAttr (applySubstAttr' subst1 a) a'
  return (subst1 <> subst2)
matchObject (Rules.MetaObject m) obj = return Subst
  { objectMetas = [(m, obj)], bindingsMetas = [], attributeMetas = [] }

matchBindings :: [Rules.Binding] -> [Phi.Binding] -> [Subst]
matchBindings [] [] = [emptySubst]
matchBindings [Rules.MetaBindings b] bindings = return Subst
  { objectMetas = [], bindingsMetas = [(b, bindings)], attributeMetas = [] }
matchBindings (p:ps) bs = do
  (bs', subst1) <- matchFindBinding p bs
  subst2 <- matchBindings ps bs'
  return (subst1 <> subst2)

-- >>> select [1,2,3,4]
-- [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]
select :: [a] -> [(a, [a])]
select [] = []
select [x] = [(x, [])]
select (x:xs) = (x, xs) :
  [ (y, x:ys)
  | (y, ys) <- select xs
  ]

matchFindBinding :: Rules.Binding -> [Phi.Binding] -> [([Phi.Binding], Subst)]
matchFindBinding p bindings =
  [ (leftover, subst)
  | (binding, leftover) <- select bindings
  , subst <- matchBinding p binding
  ]

matchBinding :: Rules.Binding -> Phi.Binding -> [Subst]
matchBinding Rules.MetaBindings{} _ = []
matchBinding (Rules.AlphaBinding a obj) (Phi.AlphaBinding a' obj') = do
  subst1 <- matchAttr a a'
  subst2 <- matchObject obj obj'
  return (subst1 <> subst2)

matchAttr :: Rules.Attribute -> Phi.Attribute -> [Subst]
matchAttr _ _ = []
