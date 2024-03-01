{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.EO.Phi.Rules.Common where

import Control.Applicative (Alternative ((<|>)), asum)
import Data.List (nubBy, sortOn)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.String (IsString (..))
import Language.EO.Phi.Syntax.Abs
import Language.EO.Phi.Syntax.Lex (Token)
import Language.EO.Phi.Syntax.Par
import Numeric (readHex, showHex)

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
  , currentAttr :: Attribute
  }

-- | A rule tries to apply a transformation to the root object, if possible.
type Rule = Context -> Object -> [Object]

applyOneRuleAtRoot :: Context -> Object -> [Object]
applyOneRuleAtRoot ctx@Context{..} obj =
  nubBy
    equalObject
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
      MetaFunction _ _ -> []

withSubObjectBindings :: (Context -> Object -> [Object]) -> Context -> [Binding] -> [[Binding]]
withSubObjectBindings _ _ [] = []
withSubObjectBindings f ctx (b : bs) =
  asum
    [ [b' : bs | b' <- withSubObjectBinding f ctx b]
    , [b : bs' | bs' <- withSubObjectBindings f ctx bs]
    ]

withSubObjectBinding :: (Context -> Object -> [Object]) -> Context -> Binding -> [Binding]
withSubObjectBinding f ctx = \case
  AlphaBinding a obj -> AlphaBinding a <$> withSubObject f (ctx{currentAttr = a}) obj
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
      nubBy
        equalObject
        [ obj''
        | obj' <- applyOneRule ctx obj
        , obj'' <- applyRules ctx obj'
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

equalBindings :: [Binding] -> [Binding] -> Bool
equalBindings bindings1 bindings2 = and (zipWith equalBinding (sortOn attr bindings1) (sortOn attr bindings2))
 where
  attr (AlphaBinding a _) = a
  attr (EmptyBinding a) = a
  attr (DeltaBinding _) = Label (LabelId "Δ")
  attr (LambdaBinding _) = Label (LabelId "λ")
  attr (MetaBindings metaId) = MetaAttr metaId

equalBinding :: Binding -> Binding -> Bool
equalBinding (AlphaBinding VTX _) (AlphaBinding VTX _) = True -- TODO #166:15min Renumerate vertices uniformly instead of ignoring them
equalBinding (AlphaBinding attr1 obj1) (AlphaBinding attr2 obj2) = attr1 == attr2 && equalObject obj1 obj2
-- Ignore deltas for now while comparing since different normalization paths can lead to different vertex data
-- TODO #120:30m normalize the deltas instead of ignoring since this actually suppresses problems
equalBinding (DeltaBinding _) (DeltaBinding _) = True
equalBinding b1 b2 = b1 == b2

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

objectBindings :: Object -> [Binding]
objectBindings (Formation bs) = bs
objectBindings (Application obj bs) = objectBindings obj ++ bs
objectBindings (ObjectDispatch obj _attr) = objectBindings obj
objectBindings _ = []

intToBytes :: Int -> Bytes
intToBytes n = Bytes $ insertDashes $ pad $ showHex n ""
 where
  pad s = (if even (length s) then "" else "0") ++ s
  insertDashes s
    | length s <= 2 = s ++ "-"
    | otherwise =
        let go = \case
              [] -> []
              [x] -> [x]
              [x, y] -> [x, y, '-']
              (x : y : xs) -> x : y : '-' : go xs
         in go s

minNu :: Int
minNu = -1

class HasMaxNu a where
  -- | get maximum vertex index
  --
  -- >>> getMaxNu @Object "⟦ a ↦ ⟦ ν ↦ ⟦ Δ ⤍ 03- ⟧ ⟧, b ↦ ⟦ ⟧ ⟧"
  -- 3
  getMaxNu :: a -> Int

instance HasMaxNu Program where
  getMaxNu :: Program -> Int
  getMaxNu (Program bindings) = getMaxNu (Formation bindings)

instance HasMaxNu Object where
  getMaxNu :: Object -> Int
  getMaxNu = \case
    Formation bindings -> maximum (minNu : (getMaxNu <$> bindings))
    Application obj bindings -> maximum (minNu : getMaxNu obj : (getMaxNu <$> bindings))
    ObjectDispatch obj _ -> getMaxNu obj
    _ -> minNu

instance HasMaxNu Binding where
  getMaxNu :: Binding -> Int
  getMaxNu = \case
    AlphaBinding VTX (Formation [DeltaBinding (Bytes bs)]) ->
      case readHex [x | x <- bs, x /= '-'] of
        [(val, "")] -> val
        _ -> error "Vertex number is incorrect"
    AlphaBinding _ obj -> getMaxNu obj
    _ -> minNu

intToBytesObject :: Int -> Object
intToBytesObject n = Formation [DeltaBinding $ intToBytes n]

nuCountAsDataObj :: Object -> Object
nuCountAsDataObj = intToBytesObject . getMaxNu
