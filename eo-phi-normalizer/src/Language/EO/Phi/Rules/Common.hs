{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.EO.Phi.Rules.Common where

import Control.Applicative (Alternative ((<|>)), asum)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.String (IsString (..))
import Language.EO.Phi.Syntax.Abs
import Language.EO.Phi.Syntax.Lex (Token)
import Language.EO.Phi.Syntax.Par

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> import Language.EO.Phi.Syntax

instance IsString Program where fromString = unsafeParseWith pProgram
instance IsString Object where fromString = unsafeParseWith pObject
instance IsString Binding where fromString = unsafeParseWith pBinding
instance IsString Attribute where fromString = unsafeParseWith pAttribute
instance IsString RuleAttribute where fromString = unsafeParseWith pRuleAttribute
instance IsString PeeledObject where fromString = unsafeParseWith pPeeledObject
instance IsString ObjectHead where fromString = unsafeParseWith pObjectHead

parseWith :: ([Token] -> Either String a) -> String -> Either String a
parseWith parser input = parser tokens
 where
  tokens = myLexer input

-- | Parse a 'Object' from a 'String'.
-- May throw an 'error` if input has a syntactical or lexical errors.
unsafeParseWith :: ([Token] -> Either String a) -> String -> a
unsafeParseWith parser input =
  case parseWith parser input of
    Left parseError -> error parseError
    Right object -> object

data Context = Context
  { allRules :: [Rule]
  , outerFormations :: NonEmpty Object
  }

-- | A rule tries to apply a transformation to the root object, if possible.
type Rule = Context -> Object -> [Object]

applyOneRuleAtRoot :: Context -> Object -> [Object]
applyOneRuleAtRoot ctx@Context{..} obj =
  [ obj'
  | rule <- allRules
  , obj' <- rule ctx obj
  ]

extendContextWith :: Object -> Context -> Context
extendContextWith obj ctx =
  ctx
    { outerFormations = obj <| outerFormations ctx
    }

withSubObject :: (Context -> Object -> [Object]) -> Context -> Object -> [Object]
withSubObject f ctx root =
  f ctx root
    <|> case root of
      Formation bindings ->
        Formation <$> withSubObjectBindings f (extendContextWith root ctx) bindings
      Application obj bindings ->
        asum
          [ Application <$> withSubObject f ctx obj <*> pure bindings
          , Application obj <$> withSubObjectBindings f ctx bindings
          ]
      ObjectDispatch obj a -> ObjectDispatch <$> withSubObject f ctx obj <*> pure a
      GlobalObject{} -> []
      ThisObject{} -> []
      Termination -> []
      MetaObject _ -> []

withSubObjectBindings :: (Context -> Object -> [Object]) -> Context -> [Binding] -> [[Binding]]
withSubObjectBindings _ _ [] = []
withSubObjectBindings f ctx (b : bs) =
  asum
    [ [b' : bs | b' <- withSubObjectBinding f ctx b]
    , [b : bs' | bs' <- withSubObjectBindings f ctx bs]
    ]

withSubObjectBinding :: (Context -> Object -> [Object]) -> Context -> Binding -> [Binding]
withSubObjectBinding f ctx = \case
  AlphaBinding a obj -> AlphaBinding a <$> withSubObject f ctx obj
  EmptyBinding{} -> []
  DeltaBinding{} -> []
  LambdaBinding{} -> []
  MetaBindings _ -> []

applyOneRule :: Context -> Object -> [Object]
applyOneRule = withSubObject applyOneRuleAtRoot

isNF :: Context -> Object -> Bool
isNF ctx = null . applyOneRule ctx

-- | Apply rules until we get a normal form.
--
-- >>> mapM_ (putStrLn . Language.EO.Phi.printTree) (applyRules (Context [rule6] ["⟦ a ↦ ⟦ b ↦ ⟦ ⟧ ⟧.b ⟧"]) "⟦ a ↦ ⟦ b ↦ ⟦ ⟧ ⟧.b ⟧.a")
applyRules :: Context -> Object -> [Object]
applyRules ctx obj
  | isNF ctx obj = [obj]
  | otherwise =
      [ obj''
      | obj' <- applyOneRule ctx obj
      , obj'' <- applyRules ctx obj'
      ]

applyRulesChain :: Context -> Object -> [[Object]]
applyRulesChain ctx obj
  | isNF ctx obj = [[obj]]
  | otherwise =
      [ obj : chain
      | obj' <- applyOneRule ctx obj
      , chain <- applyRulesChain ctx obj'
      ]

-- * Helpers

-- | Lookup a binding by the attribute name.
lookupBinding :: Attribute -> [Binding] -> Maybe Object
lookupBinding _ [] = Nothing
lookupBinding a (AlphaBinding a' object : bindings)
  | a == a' = Just object
  | otherwise = lookupBinding a bindings
lookupBinding _ _ = Nothing
