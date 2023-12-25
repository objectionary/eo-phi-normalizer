{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.Normalize (
  normalizeObjectWith,
  normalize,
  peelObject,
  unpeelObject,
) where

import Data.Maybe (fromMaybe)
import Language.EO.Phi.Syntax.Abs

data Context = Context
  { globalObject :: [Binding]
  , thisObject :: [Binding]
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
  context =
    Context
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

-- | Split compound object into its head and applications/dispatch actions.
peelObject :: Object -> PeeledObject
peelObject = \case
  Formation bindings -> PeeledObject (HeadFormation bindings) []
  Application object bindings -> peelObject object `followedBy` ActionApplication bindings
  ObjectDispatch object attr -> peelObject object `followedBy` ActionDispatch attr
  GlobalDispatch attr -> PeeledObject HeadGlobal [ActionDispatch attr]
  ThisDispatch attr -> PeeledObject HeadThis [ActionDispatch attr]
  Termination -> PeeledObject HeadTermination []
 where
  followedBy (PeeledObject object actions) action = PeeledObject object (actions ++ [action])

unpeelObject :: PeeledObject -> Object
unpeelObject (PeeledObject head_ actions) =
  case head_ of
    HeadFormation bindings -> go (Formation bindings) actions
    HeadGlobal ->
      case actions of
        ActionDispatch a : as -> go (GlobalDispatch a) as
        _ -> error "impossible: global object without dispatch!"
    HeadThis ->
      case actions of
        ActionDispatch a : as -> go (ThisDispatch a) as
        _ -> error "impossible: this object without dispatch!"
    HeadTermination -> go Termination actions
 where
  go = foldl applyAction
  applyAction object = \case
    ActionDispatch attr -> ObjectDispatch object attr
    ActionApplication bindings -> Application object bindings
