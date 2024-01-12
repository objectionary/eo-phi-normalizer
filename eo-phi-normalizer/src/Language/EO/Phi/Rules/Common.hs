{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.EO.Phi.Rules.Common where

import Language.EO.Phi.Syntax.Abs
import Control.Applicative (asum, Alternative ((<|>)))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Language.EO.Phi.Syntax

data Context = Context
  { allRules :: [Rule]
  }

-- | A rule tries to apply a transformation to the root object, if possible.
type Rule = Context -> Object -> Maybe Object

applyOneRuleAtRoot :: Context -> Object -> [Object]
applyOneRuleAtRoot ctx@Context{..} obj =
  [ obj'
  | rule <- allRules
  , Just obj' <- [rule ctx obj]
  ]

withSubObject :: (Object -> [Object]) -> Object -> [Object]
withSubObject f root = f root <|>
  case root of
    Formation bindings ->
      Formation <$> withSubObjectBindings f bindings
    Application obj bindings -> asum
      [ Application <$> withSubObject f obj <*> pure bindings
      , Application obj <$> withSubObjectBindings f bindings
      ]
    ObjectDispatch obj a -> ObjectDispatch <$> withSubObject f obj <*> pure a
    GlobalDispatch{} -> []
    ThisDispatch{} -> []
    Termination -> []

withSubObjectBindings :: (Object -> [Object]) -> [Binding] -> [[Binding]]
withSubObjectBindings _ [] = []
withSubObjectBindings f (b:bs) = asum
  [ [ b' : bs  | b'  <- withSubObjectBinding  f b ]
  , [ b  : bs' | bs' <- withSubObjectBindings f bs ]
  ]

withSubObjectBinding :: (Object -> [Object]) -> Binding -> [Binding]
withSubObjectBinding f = \case
  AlphaBinding a obj -> AlphaBinding a <$> withSubObject f obj
  EmptyBinding{} -> []
  DeltaBinding{} -> []
  LambdaBinding{} -> []

applyOneRule :: Context -> Object -> [Object]
applyOneRule = withSubObject . applyOneRuleAtRoot

isNF :: Context -> Object -> Bool
isNF ctx = null . applyOneRule ctx

-- | Apply rules until we get a normal form.
--
-- >>> mapM_ (putStrLn . Language.EO.Phi.printTree) (applyRules (Context [rule6]) "⟦ a ↦ ⟦ b ↦ ⟦ ⟧ ⟧.b ⟧.a")
-- ⟦ ⟧ (ρ ↦ ⟦ ⟧) (ρ ↦ ⟦ ⟧)
applyRules :: Context -> Object -> [Object]
applyRules ctx obj
  | isNF ctx obj = [obj]
  | otherwise =
      [ obj''
      | obj' <- applyOneRule ctx obj
      , obj'' <- applyRules ctx obj' ]

applyRulesChain :: Context -> Object -> [[Object]]
applyRulesChain ctx obj
  | isNF ctx obj = [[obj]]
  | otherwise =
      [ obj : chain
      | obj' <- applyOneRule ctx obj
      , chain <- applyRulesChain ctx obj' ]

-- * Helpers

-- | Lookup a binding by the attribute name.
lookupBinding :: Attribute -> [Binding] -> Maybe Object
lookupBinding _ [] = Nothing
lookupBinding a (AlphaBinding a' object : bindings)
  | a == a' = Just object
  | otherwise = lookupBinding a bindings
lookupBinding _ _ = Nothing
