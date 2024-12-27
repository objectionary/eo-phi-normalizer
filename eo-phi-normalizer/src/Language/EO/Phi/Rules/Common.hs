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
{-# HLINT ignore "Use &&" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.EO.Phi.Rules.Common where

import Control.Applicative (Alternative ((<|>)), asum)
import Control.Arrow (Arrow (first))
import Control.Monad
import Data.HashMap.Strict qualified as HashMap
import Data.List (minimumBy, nubBy, sortOn)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Ord (comparing)
import Language.EO.Phi.Syntax

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> import Language.EO.Phi.Syntax

-- | State of evaluation is not needed yet, but it might be in the future
type EvaluationState = ()

type NamedRule = (String, Rule)
type Atoms = HashMap.HashMap String (String -> Object -> EvaluationState -> DataizeChain (Object, EvaluationState))

data Context = Context
  { builtinRules :: Bool
  , allRules :: [NamedRule]
  , enabledAtoms :: Atoms
  , knownAtoms :: Atoms
  , outerFormations :: NonEmpty Object
  , currentAttr :: Attribute
  , insideFormation :: Bool
  -- ^ Temporary hack for applying Ksi and Phi rules when dataizing
  , insideAbstractFormation :: Bool
  , dataizePackage :: Bool
  -- ^ Temporary flag to only dataize Package attributes for the top-level formation.
  , minimizeTerms :: Bool
  , insideSubObject :: Bool
  }

sameContext :: Context -> Context -> Bool
sameContext ctx1 ctx2 =
  and
    [ outerFormations ctx1 == outerFormations ctx2
    , currentAttr ctx1 == currentAttr ctx2
    ]

-- | A rule tries to apply a transformation to the root object, if possible.
type Rule = Context -> Object -> [Object]

applyOneRuleAtRoot :: Context -> Object -> [(String, Object)]
applyOneRuleAtRoot ctx@Context{..} obj =
  nubBy
    equalObjectNamed
    [ (ruleName, obj')
    | (ruleName, rule) <- allRules
    , obj' <- rule ctx obj
    ]

extendContextWith :: Object -> Context -> Context
extendContextWith obj ctx =
  ctx
    { outerFormations = obj <| outerFormations ctx
    }

isEmptyBinding :: Binding -> Bool
isEmptyBinding EmptyBinding{} = True
isEmptyBinding DeltaEmptyBinding{} = True
isEmptyBinding _ = False

withSubObject :: (Context -> Object -> [(String, Object)]) -> Context -> Object -> [(String, Object)]
withSubObject f ctx root =
  f ctx root
    <|> go root
 where
  subctx = ctx{insideSubObject = True}
  go = \case
    Formation bindings ->
      propagateName1 Formation
        <$> withSubObjectBindings f ((extendContextWith root subctx){insideFormation = True, insideAbstractFormation = isAbstract}) bindings
     where
      isAbstract = any isEmptyBinding bindings
    Application obj bindings ->
      asum
        [ propagateName2 Application <$> withSubObject f subctx obj <*> pure bindings
        , propagateName1 (Application obj) <$> withSubObjectBindings f subctx bindings
        ]
    ObjectDispatch obj a -> propagateName2 ObjectDispatch <$> withSubObject f subctx obj <*> pure a
    GlobalObject{} -> []
    obj@GlobalObjectPhiOrg{} -> errorExpectedDesugaredObject obj
    ThisObject{} -> []
    Termination -> []
    MetaObject _ -> []
    MetaFunction _ _ -> []
    MetaTailContext{} -> []
    MetaSubstThis _ _ -> []
    MetaContextualize _ _ -> []
    ConstString{} -> []
    obj@ConstStringRaw{} -> errorExpectedDesugaredObject obj
    ConstInt{} -> []
    obj@ConstIntRaw{} -> errorExpectedDesugaredObject obj
    ConstFloat{} -> []
    obj@ConstFloatRaw{} -> errorExpectedDesugaredObject obj

-- | Given a unary function that operates only on plain objects,
-- converts it to a function that operates on named objects
propagateName1 :: (a -> b) -> (name, a) -> (name, b)
propagateName1 f (name, obj) = (name, f obj)

-- | Given a binary function that operates only on plain objects,
-- converts it to a function that operates on named objects
propagateName2 :: (a -> b -> c) -> (name, a) -> b -> (name, c)
propagateName2 f (name, obj) bs = (name, f obj bs)

withSubObjectBindings :: (Context -> Object -> [(String, Object)]) -> Context -> [Binding] -> [(String, [Binding])]
withSubObjectBindings _ _ [] = []
withSubObjectBindings f ctx (b@(AlphaBinding Rho _) : bs) =
  -- do not apply rules inside ρ-bindings
  [(name, b : bs') | (name, bs') <- withSubObjectBindings f ctx bs]
withSubObjectBindings f ctx (b : bs) =
  asum
    [ [(name, b' : bs) | (name, b') <- withSubObjectBinding f ctx b]
    , [(name, b : bs') | (name, bs') <- withSubObjectBindings f ctx bs]
    ]

withSubObjectBinding :: (Context -> Object -> [(String, Object)]) -> Context -> Binding -> [(String, Binding)]
withSubObjectBinding f ctx = \case
  AlphaBinding a obj -> propagateName1 (AlphaBinding a) <$> withSubObject f (ctx{currentAttr = a}) obj
  b@AlphaBindingSugar{} -> errorExpectedDesugaredBinding b
  EmptyBinding{} -> []
  DeltaBinding{} -> []
  DeltaEmptyBinding{} -> []
  MetaDeltaBinding{} -> []
  LambdaBinding{} -> []
  MetaBindings _ -> []

applyOneRule :: Context -> Object -> [(String, Object)]
applyOneRule = withSubObject applyOneRuleAtRoot

isNF :: Context -> Object -> Bool
isNF ctx = null . applyOneRule ctx

-- | Apply rules until we get a normal form.
applyRules :: Context -> Object -> [Object]
applyRules ctx obj = applyRulesWith (defaultApplicationLimits (objectSize obj)) ctx obj

data ApplicationLimits = ApplicationLimits
  { maxDepth :: Int
  , maxTermSize :: Int
  }

defaultApplicationLimits :: Int -> ApplicationLimits
defaultApplicationLimits sourceTermSize =
  ApplicationLimits
    { maxDepth = 130
    , maxTermSize = sourceTermSize * 10000
    }

objectSize :: Object -> Int
objectSize = \case
  Formation bindings -> 1 + sum (map bindingSize bindings)
  Application obj bindings -> 1 + objectSize obj + sum (map bindingSize bindings)
  ObjectDispatch obj _attr -> 1 + objectSize obj
  GlobalObject -> 1
  -- TODO #617:30m
  -- @fizruk, why desugar here and not assume the object is desugared?
  -- Is it because we sometimes bounce between sugared and desugared versions?
  --
  -- Should we introduce a smart constructor with a desugared object inside?
  obj@GlobalObjectPhiOrg -> errorExpectedDesugaredObject obj
  ThisObject -> 1
  Termination -> 1
  obj@MetaObject{} -> error ("impossible: expected a desugared object, but got: " <> printTree obj)
  obj@MetaFunction{} -> error ("impossible: expected a desugared object, but got: " <> printTree obj)
  obj@MetaSubstThis{} -> error ("impossible: expected a desugared object, but got: " <> printTree obj)
  obj@MetaContextualize{} -> error ("impossible: expected a desugared object, but got: " <> printTree obj)
  obj@MetaTailContext{} -> error ("impossible: expected a desugared object, but got: " <> printTree obj)
  obj@ConstString{} -> objectSize (desugar obj)
  obj@ConstStringRaw{} -> errorExpectedDesugaredObject obj
  obj@ConstInt{} -> objectSize (desugar obj)
  obj@ConstIntRaw{} -> errorExpectedDesugaredObject obj
  obj@ConstFloat{} -> objectSize (desugar obj)
  obj@ConstFloatRaw{} -> errorExpectedDesugaredObject obj

bindingSize :: Binding -> Int
bindingSize = \case
  AlphaBinding _attr obj -> objectSize obj
  EmptyBinding _attr -> 1
  DeltaBinding _bytes -> 1
  DeltaEmptyBinding -> 1
  LambdaBinding _lam -> 1
  obj@MetaDeltaBinding{} -> error ("impossible: expected a desugared object, but got: " <> printTree obj)
  obj@MetaBindings{} -> error ("impossible: expected a desugared object, but got: " <> printTree obj)
  b@AlphaBindingSugar{} -> errorExpectedDesugaredBinding b

-- | A variant of `applyRules` with a maximum application depth.
applyRulesWith :: ApplicationLimits -> Context -> Object -> [Object]
applyRulesWith limits@ApplicationLimits{..} ctx obj
  | maxDepth <= 0 = [obj]
  | isNF ctx obj = [obj]
  | otherwise =
      nubBy
        equalObject
        [ obj''
        | (_ruleName, obj') <- applyOneRule ctx obj
        , obj'' <-
            if objectSize obj' < maxTermSize
              then applyRulesWith limits{maxDepth = maxDepth - 1} ctx obj'
              else [obj']
        ]

equalProgram :: Program -> Program -> Bool
equalProgram (Program bindings1) (Program bindings2) = equalObject (Formation bindings1) (Formation bindings2)

equalObject :: Object -> Object -> Bool
equalObject (Formation bindings1) (Formation bindings2) =
  length bindings1 == length bindings2 && equalBindings bindings1 bindings2
equalObject (Application obj1 bindings1) (Application obj2 bindings2) =
  equalObject obj1 obj2 && equalBindings bindings1 bindings2
equalObject (ObjectDispatch obj1 attr1) (ObjectDispatch obj2 attr2) =
  equalObject obj1 obj2 && attr1 == attr2
equalObject obj1 obj2 = obj1 == obj2

equalObjectNamed :: (String, Object) -> (String, Object) -> Bool
equalObjectNamed x y = snd x `equalObject` snd y

equalBindings :: [Binding] -> [Binding] -> Bool
equalBindings bindings1 bindings2 = and (zipWith equalBinding (sortOn attr bindings1) (sortOn attr bindings2))
 where
  attr (AlphaBinding a _) = a
  attr (EmptyBinding a) = a
  attr (DeltaBinding _) = Label (LabelId "Δ")
  attr DeltaEmptyBinding = Label (LabelId "Δ")
  attr (MetaDeltaBinding _) = Label (LabelId "Δ")
  attr (LambdaBinding _) = Label (LabelId "λ")
  attr (MetaBindings (BindingsMetaId metaId)) = MetaAttr (LabelMetaId metaId)
  attr b@AlphaBindingSugar{} = errorExpectedDesugaredBinding b

equalBinding :: Binding -> Binding -> Bool
equalBinding (AlphaBinding attr1 obj1) (AlphaBinding attr2 obj2) = attr1 == attr2 && equalObject obj1 obj2
equalBinding b1 b2 = b1 == b2

-- * Chain variants

data LogEntry log = LogEntry
  { logEntryMessage :: String
  , logEntryLog :: log
  , logEntryLevel :: Int
  }
  deriving (Show, Functor)

newtype Chain log result = Chain
  {runChain :: Context -> [([LogEntry log], result)]}
  deriving (Functor)

type NormalizeChain = Chain Object
type DataizeChain = Chain (Either Object Bytes)
instance Applicative (Chain a) where
  pure x = Chain (const [([], x)])
  (<*>) = ap

instance Monad (Chain a) where
  return = pure
  Chain dx >>= f = Chain $ \ctx ->
    [ (steps <> steps', y)
    | (steps, x) <- dx ctx
    , (steps', y) <- runChain (f x) ctx
    ]

instance MonadFail (Chain a) where
  fail _msg = Chain (const [])

logStep :: String -> info -> Chain info ()
logStep msg info = Chain $ const [([LogEntry msg info 0], ())]

incLogLevel :: Chain info a -> Chain info a
incLogLevel (Chain k) =
  Chain $
    map (first (map (\LogEntry{..} -> LogEntry{logEntryLevel = logEntryLevel + 1, ..})))
      . k

choose :: [a] -> Chain log a
choose xs = Chain $ \_ctx -> [(mempty, x) | x <- xs]

msplit :: Chain log a -> Chain log (Maybe (a, Chain log a))
msplit (Chain m) = Chain $ \ctx ->
  case m ctx of
    [] -> runChain (return Nothing) ctx
    (logs, x) : xs -> [(logs, Just (x, Chain (const xs)))]

transformLogs :: (log1 -> log2) -> Chain log1 a -> Chain log2 a
transformLogs f (Chain normChain) = Chain $ map (first (map (fmap f))) . normChain

transformNormLogs :: NormalizeChain a -> DataizeChain a
transformNormLogs = transformLogs Left

listen :: Chain log a -> Chain log (a, [LogEntry log])
listen (Chain k) = Chain (map (\(logs, result) -> (logs, (result, logs))) . k)

minimizeObject' :: DataizeChain (Either Object Bytes) -> DataizeChain (Either Object Bytes)
minimizeObject' m = do
  fmap minimizeTerms getContext >>= \case
    True -> minimizeObject m
    False -> m

minimizeObject :: DataizeChain (Either Object Bytes) -> DataizeChain (Either Object Bytes)
minimizeObject m = do
  (x, entries) <- listen m
  case x of
    Left obj' -> do
      let objectsOnCurrentLevel =
            [logEntryLog | LogEntry{..} <- entries, logEntryLevel == 0]
      return (Left (smallestObject objectsOnCurrentLevel obj'))
    Right _ -> return x

smallestObject :: [Either Object bytes] -> Object -> Object
smallestObject objs obj = minimumBy (comparing objectSize) (obj : lefts objs)
 where
  lefts [] = []
  lefts (Left x : xs) = x : lefts xs
  lefts (Right{} : xs) = lefts xs

getContext :: Chain a Context
getContext = Chain $ \ctx -> [([], ctx)]

withContext :: Context -> Chain log a -> Chain log a
withContext = modifyContext . const

modifyContext :: (Context -> Context) -> Chain log a -> Chain log a
modifyContext g (Chain f) = Chain (f . g)

applyRulesChain' :: Context -> Object -> [([LogEntry Object], Object)]
applyRulesChain' ctx obj = applyRulesChainWith' (defaultApplicationLimits (objectSize obj)) ctx obj

-- | Apply the rules until the object is normalized, preserving the history (chain) of applications.
applyRulesChain :: Object -> NormalizeChain Object
applyRulesChain obj = applyRulesChainWith (defaultApplicationLimits (objectSize obj)) obj

applyRulesChainWith' :: ApplicationLimits -> Context -> Object -> [([LogEntry Object], Object)]
applyRulesChainWith' limits ctx obj = runChain (applyRulesChainWith limits obj) ctx

-- | A variant of `applyRulesChain` with a maximum application depth.
applyRulesChainWith :: ApplicationLimits -> Object -> NormalizeChain Object
applyRulesChainWith limits@ApplicationLimits{..} obj
  | maxDepth <= 0 = do
      logStep "Max depth hit" obj
      return obj
  | otherwise = do
      ctx <- getContext
      if isNF ctx obj
        then do
          logStep "NF" obj
          return obj
        else do
          (ruleName, obj') <- choose (applyOneRule ctx obj)
          logStep ruleName obj'
          if objectSize obj' < maxTermSize
            then applyRulesChainWith limits{maxDepth = maxDepth - 1} obj'
            else do
              logStep "Max term size hit" obj'
              return obj'

-- * Helpers

-- | Lookup a binding by the attribute name.
lookupBinding :: Attribute -> [Binding] -> Maybe Object
lookupBinding _ [] = Nothing
lookupBinding a (AlphaBinding a' object : bindings)
  | a == a' = Just object
  | otherwise = lookupBinding a bindings
lookupBinding a (_ : bindings) = lookupBinding a bindings

objectBindings :: Object -> [Binding]
objectBindings (Formation bs) = bs
objectBindings (Application obj bs) = objectBindings obj ++ bs
objectBindings (ObjectDispatch obj _attr) = objectBindings obj
objectBindings _ = []

isRhoBinding :: Binding -> Bool
isRhoBinding (AlphaBinding Rho _) = True
isRhoBinding _ = False

hideRhoInBinding :: Binding -> Binding
hideRhoInBinding = \case
  AlphaBinding a obj -> AlphaBinding a (hideRho obj)
  binding -> binding

hideRho :: Object -> Object
hideRho = \case
  Formation bindings ->
    Formation
      [ hideRhoInBinding binding
      | binding <- filter (not . isRhoBinding) bindings
      ]
  Application obj bindings ->
    Application
      (hideRho obj)
      [ hideRhoInBinding binding
      | binding <- filter (not . isRhoBinding) bindings
      ]
  ObjectDispatch obj a -> ObjectDispatch (hideRho obj) a
  obj -> obj

hideRhoInBinding1 :: Binding -> Binding
hideRhoInBinding1 = \case
  AlphaBinding a obj -> AlphaBinding a (hideRho obj)
  binding -> binding

hideRho1 :: Object -> Object
hideRho1 = \case
  Formation bindings ->
    Formation
      [ hideRhoInBinding1 binding
      | binding <- bindings
      ]
  Application obj bindings ->
    Application
      (hideRho1 obj)
      [ hideRhoInBinding1 binding
      | binding <- bindings
      ]
  ObjectDispatch obj a -> ObjectDispatch (hideRho1 obj) a
  obj -> obj
