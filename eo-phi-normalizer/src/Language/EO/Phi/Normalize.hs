{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.EO.Phi.Normalize where

import Language.EO.Phi.Syntax.Abs
import Data.Maybe (fromMaybe)

data Context = Context
  { globalObject :: [Binding]
  , thisObject   :: [Binding]
  }

lookupBinding :: Attribute -> [Binding] -> Maybe Object
lookupBinding _ [] = Nothing
lookupBinding a (AlphaBinding a' object : bindings)
  | a == a' = Just object
  | otherwise = lookupBinding a bindings
lookupBinding _ _ = Nothing

-- | Normalize an input 𝜑-program.
normalize :: Program -> Program
normalize (Program bindings) = Program (map (normalizeBindingWith context) bindings)
  where
    context = Context
      { globalObject = bindings
      , thisObject = bindings
      }

normalizeBindingWith :: Context -> Binding -> Binding
normalizeBindingWith context = \case
  AlphaBinding a object -> AlphaBinding a (normalizeObjectWith context object)
  binding -> binding

normalizeObjectWith :: Context -> Object -> Object
normalizeObjectWith Context{..} object =
  case object of
    ThisDispatch a ->
      fromMaybe object (lookupBinding a thisObject)
    _ -> object
