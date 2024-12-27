{- FOURMOLU_DISABLE -}
-- The MIT License (MIT)

-- Copyright (c) 2016-2024 Objectionary.com

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{- FOURMOLU_ENABLE -}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-forall-identifier #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Language.EO.Phi.Rules.Yaml where

import Control.Monad (guard, unless)
import Control.Monad.State (State, evalState)
import Data.Aeson (FromJSON (..), Options (sumEncoding), SumEncoding (UntaggedValue), genericParseJSON)
import Data.Aeson.Types (defaultOptions)
import Data.Coerce (coerce)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)

import Language.EO.Phi.Rules.Common (Context (..), NamedRule)
import Language.EO.Phi.Rules.Common qualified as Common
import Language.EO.Phi.Syntax
import PyF (fmt)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists

instance FromJSON Object where parseJSON = fmap fromString . parseJSON
instance FromJSON Binding where parseJSON = fmap fromString . parseJSON

instance FromJSON ObjectMetaId where parseJSON = fmap ObjectMetaId . parseJSON
instance FromJSON LabelMetaId where parseJSON = fmap LabelMetaId . parseJSON
instance FromJSON TailMetaId where parseJSON = fmap TailMetaId . parseJSON
instance FromJSON BindingsMetaId where parseJSON = fmap BindingsMetaId . parseJSON
instance FromJSON BytesMetaId where parseJSON = fmap BytesMetaId . parseJSON

instance FromJSON MetaId where
  parseJSON = fmap fromString . parseJSON

instance FromJSON Attribute where parseJSON = fmap fromString . parseJSON
instance FromJSON RuleAttribute where parseJSON = fmap fromString . parseJSON

instance FromJSON LabelId
instance FromJSON AlphaIndex

data RuleSet = RuleSet
  { title :: String
  , rules :: [Rule]
  }
  deriving (Generic, FromJSON, Show)

data RuleContext = RuleContext
  { global_object :: Maybe Object
  , current_object :: Maybe Object
  , current_attribute :: Maybe Attribute
  }
  deriving (Generic, FromJSON, Show)

data Rule = Rule
  { name :: String
  , description :: String
  , context :: Maybe RuleContext
  , forall :: Maybe [MetaId]
  , pattern :: Object
  , result :: Object
  , fresh :: Maybe [FreshMetaId]
  , when :: Maybe [Condition]
  , tests :: Maybe [RuleTest]
  }
  deriving (Generic, FromJSON, Show)

data FreshMetaId = FreshMetaId
  { name :: LabelMetaId
  , prefix :: Maybe String
  }
  deriving (Generic, FromJSON, Show)

data RuleTest = RuleTest
  { name :: String
  , input :: Object
  , output :: [Object]
  , options :: Maybe [RuleTestOption]
  }
  deriving (Generic, FromJSON, Show)

newtype RuleTestOption = TakeOne {take_one :: Bool}
  -- deriving (Generic, Show, FromJSON)
  deriving (Eq, Generic, Show)
instance FromJSON RuleTestOption where
  parseJSON = genericParseJSON defaultOptions{sumEncoding = UntaggedValue}

data AttrsInBindings = AttrsInBindings
  { attrs :: [RuleAttribute]
  , bindings :: [Binding]
  }
  deriving (Generic, Show, FromJSON)
data Condition
  = IsNF {nf :: Object}
  | IsNFInsideFormation {nf_inside_formation :: Object}
  | PresentAttrs {present_attrs :: AttrsInBindings}
  | AbsentAttrs {absent_attrs :: AttrsInBindings}
  | AttrNotEqual {not_equal :: (Attribute, Attribute)}
  | ApplyInSubformations {apply_in_subformations :: Bool}
  | ApplyInAbstractSubformations {apply_in_abstract_subformations :: Bool}
  deriving (Generic, Show)
instance FromJSON Condition where
  parseJSON = genericParseJSON defaultOptions{sumEncoding = UntaggedValue}

parseRuleSetFromFile :: FilePath -> IO RuleSet
parseRuleSetFromFile = Yaml.decodeFileThrow

convertRule :: Rule -> Common.Rule
convertRule Rule{..} ctx obj = do
  -- first validate pattern and result in the rule
  -- TODO: we should perform this check once, not every time we run the rule
  let freshMetaIds =
        Set.mapMonotonic MetaIdLabel $
          foldMap (Set.fromList . map (\FreshMetaId{name = x} -> x)) fresh

      patternMetaIds = objectMetaIds pattern
      resultMetaIds = objectMetaIds result

      unusedFreshMetaIds = Set.difference freshMetaIds resultMetaIds

      ppMetaIds = intercalate ", " . map printTree . Set.toList

  unless (null unusedFreshMetaIds) $
    error ("invalid rule: result does not use some fresh variables quantified by the fresh: " <> ppMetaIds unusedFreshMetaIds)

  case forall of
    Nothing -> return ()
    Just forall' -> do
      let forallMetaIds = Set.fromList forall'
          resultAllowedMetaIds = forallMetaIds <> freshMetaIds
          unquantifiedMetaIds = Set.difference patternMetaIds forallMetaIds
          unusedMetaIds = Set.difference forallMetaIds patternMetaIds
          unquantifiedResultMetaIds = Set.difference resultMetaIds resultAllowedMetaIds
      unless (null unquantifiedMetaIds) $
        error ("invalid rule: pattern uses meta variables not quantified by the forall: " <> ppMetaIds unquantifiedMetaIds)
      unless (null unusedMetaIds) $
        error ("invalid rule: pattern does not use some variables quantified by the forall: " <> ppMetaIds unusedMetaIds)
      unless (null unquantifiedResultMetaIds) $
        error ("invalid rule: result uses meta variables not quantified by the forall or the fresh: " <> ppMetaIds unquantifiedResultMetaIds)

  contextSubsts <- matchContext ctx context
  let pattern' = applySubst contextSubsts pattern
      result' = applySubst contextSubsts result
  subst <- matchObject pattern' obj
  guard $ all (\cond -> checkCond ctx cond (contextSubsts <> subst)) (fromMaybe [] when)
  let substFresh = mkFreshSubst ctx result' fresh
      result'' = applySubst (contextSubsts <> subst <> substFresh) result'
      -- TODO #152:30m what context should we pass to evaluate meta funcs?
      obj' = evaluateMetaFuncs obj result''
  guard $ not (objectHasMetavars obj')
  pure obj'

convertRuleNamed :: Rule -> NamedRule
convertRuleNamed rule = (rule.name, convertRule rule)

mkFreshSubst :: Context -> Object -> Maybe [FreshMetaId] -> Subst
mkFreshSubst ctx obj metas =
  Subst
    { objectMetas = []
    , bindingsMetas = []
    , attributeMetas = mkFreshAttributes (usedLabelIds ctx <> objectLabelIds obj) (fromMaybe [] metas)
    , bytesMetas = []
    , contextMetas = []
    }

mkFreshAttributes :: Set LabelId -> [FreshMetaId] -> [(LabelMetaId, Attribute)]
mkFreshAttributes _ids [] = []
mkFreshAttributes ids (x : xs) =
  case mkFreshAttribute ids x of
    (ma, ids') -> ma : mkFreshAttributes ids' xs

mkFreshAttribute :: Set LabelId -> FreshMetaId -> ((LabelMetaId, Attribute), Set LabelId)
mkFreshAttribute ids FreshMetaId{..} = ((name, Label label), Set.insert label ids)
 where
  label =
    head
      [ l
      | i <- [1 ..]
      , let l = LabelId (fromMaybe "tmp" prefix <> "$" <> show i)
      , l `Set.notMember` ids
      ]

usedLabelIds :: Context -> Set LabelId
usedLabelIds Context{..} = objectLabelIds globalObject
 where
  globalObject = NonEmpty.last outerFormations

objectLabelIds :: Object -> Set LabelId
objectLabelIds = \case
  GlobalObject -> mempty
  obj@GlobalObjectPhiOrg -> errorExpectedDesugaredObject obj
  ThisObject -> mempty
  Formation bindings -> foldMap bindingLabelIds bindings
  ObjectDispatch obj a -> objectLabelIds obj <> attrLabelIds a
  Application obj bindings -> objectLabelIds obj <> foldMap bindingLabelIds bindings
  Termination -> mempty
  MetaObject{} -> mempty
  MetaFunction _ obj -> objectLabelIds obj
  MetaTailContext obj _ -> objectLabelIds obj
  MetaSubstThis obj obj' -> objectLabelIds obj <> objectLabelIds obj'
  MetaContextualize obj obj' -> objectLabelIds obj <> objectLabelIds obj'
  obj@ConstString{} -> objectLabelIds (desugar obj)
  obj@ConstStringRaw{} -> errorExpectedDesugaredObject obj
  obj@ConstInt{} -> objectLabelIds (desugar obj)
  obj@ConstIntRaw{} -> errorExpectedDesugaredObject obj
  obj@ConstFloat{} -> objectLabelIds (desugar obj)
  obj@ConstFloatRaw{} -> errorExpectedDesugaredObject obj

bindingLabelIds :: Binding -> Set LabelId
bindingLabelIds = \case
  AlphaBinding a obj -> objectLabelIds obj <> attrLabelIds a
  DeltaBinding _bytes -> mempty
  EmptyBinding a -> attrLabelIds a
  DeltaEmptyBinding -> mempty
  LambdaBinding _ -> mempty
  MetaBindings _ -> mempty
  MetaDeltaBinding _ -> mempty
  b@AlphaBindingSugar{} -> errorExpectedDesugaredBinding b

attrLabelIds :: Attribute -> Set LabelId
attrLabelIds (Label l) = Set.singleton l
attrLabelIds _ = mempty

-- >>> matchContext (Context [] ["⟦ a ↦ ⟦ ⟧, x ↦ ξ.a ⟧"] (Label (LabelId "x"))) (Just (RuleContext Nothing (Just "⟦ !a ↦ !obj, !B ⟧") (Just "!a")))
-- [Subst {
--   objectMetas = [!obj -> 'ξ.a']
--   bindingsMetas = [!B -> 'a ↦ ⟦ ⟧']
--   attributeMetas = [!a -> 'x']
-- }]
matchContext :: Common.Context -> Maybe RuleContext -> [Subst]
matchContext Common.Context{} Nothing = [emptySubst]
matchContext Common.Context{..} (Just (RuleContext{..})) = do
  subst1 <- maybe [emptySubst] (`matchObject` globalObject) global_object
  subst2 <- maybe [emptySubst] ((`matchObject` thisObject) . applySubst subst1) current_object
  subst3 <- maybe [emptySubst] ((`matchAttr` currentAttr) . applySubstAttr (subst1 <> subst2)) current_attribute
  return (subst1 <> subst2 <> subst3)
 where
  globalObject = NonEmpty.last outerFormations
  thisObject = NonEmpty.head outerFormations

objectMetaIds :: Object -> Set MetaId
objectMetaIds (Formation bindings) = foldMap bindingMetaIds bindings
objectMetaIds (Application object bindings) = objectMetaIds object <> foldMap bindingMetaIds bindings
objectMetaIds (ObjectDispatch object attr) = objectMetaIds object <> attrMetaIds attr
objectMetaIds GlobalObject = mempty
objectMetaIds obj@GlobalObjectPhiOrg = errorExpectedDesugaredObject obj
objectMetaIds ThisObject = mempty
objectMetaIds Termination = mempty
objectMetaIds (MetaObject x) = Set.singleton (MetaIdObject x)
objectMetaIds (MetaFunction _ obj) = objectMetaIds obj
objectMetaIds (MetaTailContext obj x) = objectMetaIds obj <> Set.singleton (MetaIdTail x)
objectMetaIds (MetaSubstThis obj obj') = foldMap objectMetaIds [obj, obj']
objectMetaIds (MetaContextualize obj obj') = foldMap objectMetaIds [obj, obj']
objectMetaIds obj@ConstString{} = objectMetaIds (desugar obj)
objectMetaIds obj@ConstStringRaw{} = errorExpectedDesugaredObject obj
objectMetaIds obj@ConstInt{} = objectMetaIds (desugar obj)
objectMetaIds obj@ConstIntRaw{} = errorExpectedDesugaredObject obj
objectMetaIds obj@ConstFloat{} = objectMetaIds (desugar obj)
objectMetaIds obj@ConstFloatRaw{} = errorExpectedDesugaredObject obj

bindingMetaIds :: Binding -> Set MetaId
bindingMetaIds (AlphaBinding attr obj) = attrMetaIds attr <> objectMetaIds obj
bindingMetaIds (EmptyBinding attr) = attrMetaIds attr
bindingMetaIds (DeltaBinding _) = mempty
bindingMetaIds DeltaEmptyBinding = mempty
bindingMetaIds (LambdaBinding _) = mempty
bindingMetaIds (MetaBindings x) = Set.singleton (MetaIdBindings x)
bindingMetaIds (MetaDeltaBinding x) = Set.singleton (MetaIdBytes x)
bindingMetaIds b@AlphaBindingSugar{} = errorExpectedDesugaredBinding b

attrMetaIds :: Attribute -> Set MetaId
attrMetaIds Phi = mempty
attrMetaIds Rho = mempty
attrMetaIds (Label _) = mempty
attrMetaIds (Alpha _) = mempty
attrMetaIds (MetaAttr x) = Set.singleton (MetaIdLabel x)
attrMetaIds a@(AttrSugar{}) = errorExpectedDesugaredAttribute a
attrMetaIds a@(PhiSugar{}) = errorExpectedDesugaredAttribute a

objectHasMetavars :: Object -> Bool
objectHasMetavars (Formation bindings) = any bindingHasMetavars bindings
objectHasMetavars (Application object bindings) = objectHasMetavars object || any bindingHasMetavars bindings
objectHasMetavars (ObjectDispatch object attr) = objectHasMetavars object || attrHasMetavars attr
objectHasMetavars GlobalObject = False
objectHasMetavars obj@GlobalObjectPhiOrg = errorExpectedDesugaredObject obj
objectHasMetavars ThisObject = False
objectHasMetavars Termination = False
objectHasMetavars (MetaObject _) = True
objectHasMetavars (MetaFunction _ _) = True
objectHasMetavars MetaTailContext{} = True
objectHasMetavars (MetaSubstThis _ _) = True -- technically not a metavar, but a substitution
objectHasMetavars (MetaContextualize _ _) = True
objectHasMetavars obj@ConstString{} = objectHasMetavars (desugar obj)
objectHasMetavars obj@ConstStringRaw{} = errorExpectedDesugaredObject obj
objectHasMetavars obj@ConstInt{} = objectHasMetavars (desugar obj)
objectHasMetavars obj@ConstIntRaw{} = errorExpectedDesugaredObject obj
objectHasMetavars obj@ConstFloat{} = objectHasMetavars (desugar obj)
objectHasMetavars obj@ConstFloatRaw{} = errorExpectedDesugaredObject obj

bindingHasMetavars :: Binding -> Bool
bindingHasMetavars (AlphaBinding attr obj) = attrHasMetavars attr || objectHasMetavars obj
bindingHasMetavars (EmptyBinding attr) = attrHasMetavars attr
bindingHasMetavars (DeltaBinding _) = False
bindingHasMetavars DeltaEmptyBinding = False
bindingHasMetavars (LambdaBinding _) = False
bindingHasMetavars (MetaBindings _) = True
bindingHasMetavars (MetaDeltaBinding _) = True
bindingHasMetavars b@AlphaBindingSugar{} = errorExpectedDesugaredBinding b

attrHasMetavars :: Attribute -> Bool
attrHasMetavars Phi = False
attrHasMetavars Rho = False
attrHasMetavars (Label _) = False
attrHasMetavars (Alpha _) = False
attrHasMetavars (MetaAttr _) = True
attrHasMetavars a@AttrSugar{} = errorExpectedDesugaredAttribute a
attrHasMetavars a@PhiSugar{} = errorExpectedDesugaredAttribute a

-- | Given a condition, and a substition from object matching
--   tells whether the condition matches the object
checkCond :: Common.Context -> Condition -> Subst -> Bool
checkCond ctx (IsNF obj) subst = Common.isNF ctx $ applySubst subst obj
checkCond ctx (IsNFInsideFormation obj) subst = Common.isNF ctx{insideFormation = True} $ applySubst subst obj
checkCond _ctx (PresentAttrs (AttrsInBindings attrs bindings)) subst = any (`hasAttr` substitutedBindings) substitutedAttrs
 where
  substitutedBindings = concatMap (applySubstBinding subst) bindings
  ruleToNormalAttr :: RuleAttribute -> Attribute
  ruleToNormalAttr (ObjectAttr a) = a
  -- Hack to be able to use applySubstAttr with RuleAttribute.
  -- Should not actually substitute anything anyway since they are not metavariables
  ruleToNormalAttr DeltaAttr = Label (LabelId "Δ")
  ruleToNormalAttr LambdaAttr = Label (LabelId "λ")
  normalToRuleAttr :: Attribute -> RuleAttribute
  normalToRuleAttr (Label (LabelId "Δ")) = DeltaAttr
  normalToRuleAttr (Label (LabelId "λ")) = LambdaAttr
  normalToRuleAttr a = ObjectAttr a
  substitutedAttrs = map (normalToRuleAttr . applySubstAttr subst . ruleToNormalAttr) attrs
checkCond ctx (AbsentAttrs s) subst = not $ checkCond ctx (PresentAttrs s) subst
checkCond _ctx (AttrNotEqual (a1, a2)) subst = applySubstAttr subst a1 /= applySubstAttr subst a2
checkCond ctx (ApplyInSubformations shouldApply) _subst
  | shouldApply = True
  | otherwise = not (insideFormation ctx)
checkCond ctx (ApplyInAbstractSubformations shouldApply) _subst
  | shouldApply = True
  | otherwise = not (insideAbstractFormation ctx)

hasAttr :: RuleAttribute -> [Binding] -> Bool
hasAttr attr = any (isAttr attr)
 where
  isAttr (ObjectAttr a) (AlphaBinding a' _) = a == a'
  isAttr (ObjectAttr a) (EmptyBinding a') = a == a'
  isAttr DeltaAttr (DeltaBinding _) = True
  isAttr DeltaAttr DeltaEmptyBinding = True
  isAttr LambdaAttr (LambdaBinding _) = True
  isAttr _ _ = False

-- input: ⟦ a ↦ ⟦ c ↦ ⟦ ⟧ ⟧, b ↦ ⟦ ⟧ ⟧.a

-- pattern:   ⟦ !a ↦ !n, !B ⟧.!a
-- result:    !n(ρ ↦ ⟦ !B ⟧)

-- match pattern input (get substitution):
--  !a = a
--  !n = ⟦ c ↦ ⟦ ⟧ ⟧
--  !B = b ↦ ⟦ ⟧

-- actual result (after applying substitution):
--  ⟦ c ↦ ⟦ ⟧ ⟧(ρ ↦ ⟦ b ↦ ⟦ ⟧ ⟧)

data OneHoleContext = OneHoleContext
  { holeMetaId :: !ObjectMetaId
  , contextObject :: !Object
  }
  deriving (Show)

data Subst = Subst
  { objectMetas :: [(ObjectMetaId, Object)]
  , bindingsMetas :: [(BindingsMetaId, [Binding])]
  , attributeMetas :: [(LabelMetaId, Attribute)]
  , bytesMetas :: [(BytesMetaId, Bytes)]
  , contextMetas :: [(TailMetaId, OneHoleContext)]
  }
instance Show Subst where
  show Subst{..} =
    intercalate
      "\n"
      [ "Subst {"
      , "  objectMetas = [" <> showMappings objectMetas <> "]"
      , "  bindingsMetas = [" <> showMappings bindingsMetas <> "]"
      , "  attributeMetas = [" <> showMappings attributeMetas <> "]"
      , "  bytesMetas = [" <> showMappings bytesMetas <> "]"
      , "  contextMetas = [" <> show contextMetas <> "]"
      , "}"
      ]
   where
    showMappings metas = intercalate "; " $ map (\(metaId, obj) -> [fmt|{printTree metaId} -> '{printTree obj}'|]) metas

instance Semigroup Subst where
  (<>) = mergeSubst

instance Monoid Subst where
  mempty = emptySubst

emptySubst :: Subst
emptySubst = Subst [] [] [] [] []

-- >>> putStrLn $ Language.EO.Phi.printTree (applySubst (Subst [("!n", "⟦ c ↦ ⟦ ⟧ ⟧")] [("!B", ["b ↦ ⟦ ⟧"])] [("!a", "a")]) "!n(ρ ↦ ⟦ !B ⟧)" :: Object)
-- ⟦ c ↦ ⟦ ⟧ ⟧ (ρ ↦ ⟦ b ↦ ⟦ ⟧ ⟧)
applySubst :: Subst -> Object -> Object
applySubst subst@Subst{..} = \case
  Formation bindings ->
    Formation (applySubstBindings subst bindings)
  Application obj bindings ->
    Application (applySubst subst obj) (applySubstBindings subst bindings)
  ObjectDispatch obj a ->
    ObjectDispatch (applySubst subst obj) (applySubstAttr subst a)
  GlobalObject -> GlobalObject
  obj@GlobalObjectPhiOrg -> errorExpectedDesugaredObject obj
  ThisObject -> ThisObject
  obj@(MetaObject x) -> fromMaybe obj $ lookup x objectMetas
  Termination -> Termination
  MetaSubstThis obj thisObj -> MetaSubstThis (applySubst subst obj) (applySubst subst thisObj)
  MetaContextualize obj thisObj -> MetaContextualize (applySubst subst obj) (applySubst subst thisObj)
  obj@MetaFunction{} -> obj
  MetaTailContext obj c ->
    case lookup c contextMetas of
      Nothing -> MetaTailContext (applySubst subst obj) c
      Just OneHoleContext{..} ->
        let holeSubst = mempty{objectMetas = [(holeMetaId, applySubst subst obj)]}
         in applySubst holeSubst contextObject
  obj@ConstString{} -> applySubst subst (desugar obj)
  obj@ConstStringRaw{} -> errorExpectedDesugaredObject obj
  obj@ConstInt{} -> applySubst subst (desugar obj)
  obj@ConstIntRaw{} -> errorExpectedDesugaredObject obj
  obj@ConstFloat{} -> applySubst subst (desugar obj)
  obj@ConstFloatRaw{} -> errorExpectedDesugaredObject obj

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
  DeltaEmptyBinding -> [DeltaEmptyBinding]
  LambdaBinding bytes -> [LambdaBinding (coerce bytes)]
  b@(MetaBindings m) -> fromMaybe [b] (lookup m bindingsMetas)
  b@(MetaDeltaBinding m) -> maybe [b] (pure . DeltaBinding) (lookup m bytesMetas)
  b@AlphaBindingSugar{} -> errorExpectedDesugaredBinding b

mergeSubst :: Subst -> Subst -> Subst
mergeSubst (Subst xs ys zs ws us) (Subst xs' ys' zs' ws' us') =
  Subst (xs ++ xs') (ys ++ ys') (zs ++ zs') (ws ++ ws') (us ++ us')

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
  pure emptySubst{objectMetas = [(m, obj)]}
matchObject (MetaTailContext pat x) obj = do
  (subst@Subst{..}, matchedCtx) <- matchOneHoleContext x pat obj
  return subst{contextMetas = contextMetas <> [(x, matchedCtx)]}
matchObject Termination Termination = [emptySubst]
matchObject ThisObject ThisObject = [emptySubst]
matchObject GlobalObject GlobalObject = [emptySubst]
matchObject _ _ = [] -- ? emptySubst ?

matchOneHoleContext :: TailMetaId -> Object -> Object -> [(Subst, OneHoleContext)]
matchOneHoleContext ctxId pat obj = matchWhole <> matchPart
 where
  TailMetaId name = ctxId
  holeId = ObjectMetaId (name ++ ":hole") -- FIXME: ensure fresh names
  matchWhole = do
    subst' <- matchObject pat obj
    pure (subst', OneHoleContext holeId (MetaObject holeId))
  matchPart = case obj of
    ObjectDispatch obj' a -> do
      (subst, OneHoleContext{..}) <- matchOneHoleContext ctxId pat obj'
      return (subst, OneHoleContext{contextObject = ObjectDispatch contextObject a, ..})
    -- FIXME: consider matching inside bindings of application as well
    Application obj' bindings -> do
      (subst, OneHoleContext{..}) <- matchOneHoleContext ctxId pat obj'
      return (subst, OneHoleContext{contextObject = Application contextObject bindings, ..})
    -- cases below cannot be matched
    Formation{} -> []
    GlobalObject -> []
    ThisObject -> []
    Termination -> []
    ConstString{} -> []
    ConstStringRaw{} -> errorExpectedDesugaredObject obj
    ConstInt{} -> []
    ConstIntRaw{} -> errorExpectedDesugaredObject obj
    ConstFloat{} -> []
    ConstFloatRaw{} -> errorExpectedDesugaredObject obj
    -- TODO #617:30m Should cases below be errors?
    GlobalObjectPhiOrg -> errorExpectedDesugaredObject obj
    MetaSubstThis{} -> []
    MetaContextualize{} -> []
    MetaObject{} -> []
    MetaTailContext{} -> []
    MetaFunction{} -> []

-- | Evaluate meta functions
-- given top-level context as an object
-- and an object
evaluateMetaFuncs :: Object -> Object -> Object
evaluateMetaFuncs _obj' obj =
  evalState
    (evaluateMetaFuncs' obj)
    MetaState{}

data MetaState = MetaState
  {
  }
  deriving (Generic)

evaluateMetaFuncs' :: Object -> State MetaState Object
evaluateMetaFuncs' (Formation bindings) = Formation <$> mapM evaluateMetaFuncsBinding bindings
evaluateMetaFuncs' (Application obj bindings) = Application <$> evaluateMetaFuncs' obj <*> mapM evaluateMetaFuncsBinding bindings
evaluateMetaFuncs' (ObjectDispatch obj a) = ObjectDispatch <$> evaluateMetaFuncs' obj <*> pure a
evaluateMetaFuncs' (MetaSubstThis obj thisObj) = evaluateMetaFuncs' (substThis thisObj obj)
evaluateMetaFuncs' (MetaContextualize obj thisObj) = evaluateMetaFuncs' (contextualize thisObj obj)
evaluateMetaFuncs' obj = pure obj

evaluateMetaFuncsBinding :: Binding -> State MetaState Binding
evaluateMetaFuncsBinding (AlphaBinding attr obj) = AlphaBinding attr <$> evaluateMetaFuncs' obj
evaluateMetaFuncsBinding binding = pure binding

matchBindings :: [Binding] -> [Binding] -> [Subst]
matchBindings [] [] = [emptySubst]
matchBindings [MetaBindings b] bindings =
  pure
    emptySubst
      { bindingsMetas = [(b, bindings)]
      }
matchBindings (p : ps) bs = do
  (bs', subst1) <- matchFindBinding p bs
  subst2 <- matchBindings (applySubstBindings subst1 ps) bs'
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
matchBinding (MetaDeltaBinding m) (DeltaBinding bytes) = [emptySubst{bytesMetas = [(m, bytes)]}]
matchBinding (DeltaBinding bytes) (DeltaBinding bytes')
  | bytes == bytes' = [emptySubst]
matchBinding DeltaEmptyBinding DeltaEmptyBinding = [emptySubst]
matchBinding (EmptyBinding a1) (EmptyBinding a2) = matchAttr a1 a2
matchBinding (LambdaBinding f1) (LambdaBinding f2)
  | f1 == f2 = [emptySubst]
matchBinding _ _ = []

matchAttr :: Attribute -> Attribute -> [Subst]
matchAttr l r | l == r = [emptySubst]
matchAttr (MetaAttr metaId) attr =
  [ emptySubst
      { attributeMetas = [(metaId, attr)]
      }
  ]
matchAttr _ _ = []

substThis :: Object -> Object -> Object
substThis thisObj = go
 where
  isAttachedRho (AlphaBinding Rho _) = True
  isAttachedRho _ = False

  isEmptyRho (EmptyBinding Rho) = True
  isEmptyRho _ = False

  go = \case
    ThisObject -> thisObj -- ξ is substituted
    -- IMPORTANT: we are injecting a ρ-attribute in formations!
    obj@(Formation bindings)
      | any isAttachedRho bindings -> obj
      | otherwise -> Formation (filter (not . isEmptyRho) bindings ++ [AlphaBinding Rho thisObj])
    -- everywhere else we simply recursively traverse the φ-term
    Application obj bindings -> Application (go obj) (map (substThisBinding thisObj) bindings)
    ObjectDispatch obj a -> ObjectDispatch (go obj) a
    GlobalObject -> GlobalObject
    obj@GlobalObjectPhiOrg -> errorExpectedDesugaredObject obj
    Termination -> Termination
    obj@MetaTailContext{} -> error ("impossible: trying to substitute ξ in " <> printTree obj)
    obj@MetaContextualize{} -> error ("impossible: trying to substitute ξ in " <> printTree obj)
    obj@MetaSubstThis{} -> error ("impossible: trying to substitute ξ in " <> printTree obj)
    obj@MetaObject{} -> error ("impossible: trying to substitute ξ in " <> printTree obj)
    obj@MetaFunction{} -> error ("impossible: trying to substitute ξ in " <> printTree obj)
    obj@ConstString{} -> obj
    obj@ConstStringRaw{} -> errorExpectedDesugaredObject obj
    obj@ConstInt{} -> obj
    obj@ConstIntRaw{} -> errorExpectedDesugaredObject obj
    obj@ConstFloat{} -> obj
    obj@ConstFloatRaw{} -> errorExpectedDesugaredObject obj

substThisBinding :: Object -> Binding -> Binding
substThisBinding obj = \case
  AlphaBinding a obj' -> AlphaBinding a (substThis obj obj')
  EmptyBinding a -> EmptyBinding a
  DeltaBinding bytes -> DeltaBinding bytes
  DeltaEmptyBinding -> DeltaEmptyBinding
  LambdaBinding bytes -> LambdaBinding bytes
  b@MetaBindings{} -> error ("impossible: trying to substitute ξ in " <> printTree b)
  b@MetaDeltaBinding{} -> error ("impossible: trying to substitute ξ in " <> printTree b)
  b@AlphaBindingSugar{} -> errorExpectedDesugaredBinding b

contextualize :: Object -> Object -> Object
contextualize thisObj = go
 where
  go = \case
    ThisObject -> thisObj -- ξ is substituted
    obj@(Formation _bindings) -> obj
    ObjectDispatch obj a -> ObjectDispatch (go obj) a
    Application obj bindings -> Application (go obj) (map (contextualizeBinding thisObj) bindings)
    GlobalObject -> GlobalObject -- TODO: Change to what GlobalObject is attached to
    obj@GlobalObjectPhiOrg -> errorExpectedDesugaredObject obj
    Termination -> Termination
    obj@MetaTailContext{} -> error ("impossible: trying to contextualize " <> printTree obj)
    obj@MetaContextualize{} -> error ("impossible: trying to contextualize " <> printTree obj)
    obj@MetaSubstThis{} -> error ("impossible: trying to contextualize " <> printTree obj)
    obj@MetaObject{} -> error ("impossible: trying to contextualize " <> printTree obj)
    obj@MetaFunction{} -> error ("impossible: trying to contextualize " <> printTree obj)
    obj@ConstString{} -> go (desugar obj)
    obj@ConstStringRaw{} -> errorExpectedDesugaredObject obj
    obj@ConstInt{} -> go (desugar obj)
    obj@ConstIntRaw{} -> errorExpectedDesugaredObject obj
    obj@ConstFloat{} -> go (desugar obj)
    obj@ConstFloatRaw{} -> errorExpectedDesugaredObject obj

contextualizeBinding :: Object -> Binding -> Binding
contextualizeBinding obj = \case
  AlphaBinding a obj' -> AlphaBinding a (contextualize obj obj')
  EmptyBinding a -> EmptyBinding a
  DeltaBinding bytes -> DeltaBinding bytes
  DeltaEmptyBinding -> DeltaEmptyBinding
  LambdaBinding bytes -> LambdaBinding bytes
  b@MetaBindings{} -> error ("impossible: trying to contextualize " <> printTree b)
  b@MetaDeltaBinding{} -> error ("impossible: trying to contextualize " <> printTree b)
  b@AlphaBindingSugar{} -> errorExpectedDesugaredBinding b
