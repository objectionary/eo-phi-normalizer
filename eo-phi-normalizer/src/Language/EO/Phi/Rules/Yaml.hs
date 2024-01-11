{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Language.EO.Phi.Rules.Yaml where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..))
import qualified Data.Yaml as Yaml
import Data.String (IsString(..))
import qualified Language.EO.Phi.Rules.Syntax.Abs as Rules
import qualified Language.EO.Phi.Rules.Syntax.Par as Rules

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
convertRule Rule{..} ctx obj =
  [ obj'
  | subst <- match pattern obj
  , let obj' = applySubst subst result
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
  { objectMetas     :: [(MetaId, Phi.Object)]
  , bindingsMetas   :: [(MetaId, [Phi.Binding])]
  , attributeMetas  :: [(MetaId, Phi.Attribute)]
  }

emptySubst :: Subst
emptySubst = Subst [] [] []

mergeSubst :: Subst -> Subst -> Subst

match :: Rules.Object -> Phi.Object -> Subst
match m@MetaId{} obj = Subst { objectMetas = [(m, obj)], bindingsMetas = [], attributeMetas = [] }
match
