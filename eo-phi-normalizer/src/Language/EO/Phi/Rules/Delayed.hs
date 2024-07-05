{-# LANGUAGE LambdaCase #-}

module Language.EO.Phi.Rules.Delayed where

import Data.List (partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Syntax.Abs

-- $setup
-- >>> :set -XOverloadedStrings

data ObjectClosure
  = FormationC Env [Binding] [BindingClosure]
  | ApplicationC ObjectClosure [BindingClosure]
  | ObjectDispatchC ObjectClosure Attribute
  | TerminationC
  deriving (Eq)

data BindingClosure
  = AlphaBindingC Attribute ObjectClosure
  | DeltaBindingC Bytes
  deriving (Eq)

type Env =
  NonEmpty
    ObjectClosure

-- ⟦ φ ↦ ξ.ρ.eq(α0 ↦ Φ.org.eolang.bytes(Δ ⤍ 01-)) ⟧.ρ.eq(α0 ↦ Φ.org.eolang.bytes(Δ ⤍ 01-))

-- Application (ObjectDispatch (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "true"))) (Label (LabelId "eq"))) [AlphaBinding (Alpha (AlphaIndex "\945\&0")) (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "true")))]

-- Φ.org.eolang.true.eq(α0 ↦ Φ.org.eolang.true)

evalContext :: Context -> Env
evalContext ctx = go (outerFormations ctx)
 where
  go :: NonEmpty Object -> NonEmpty ObjectClosure
  go (Formation bindings :| []) = globalClosure :| []
   where
    globalClosure = FormationC (globalClosure :| []) bindings []
  go (_ :| []) = error "non-formation as global object"
  go (x :| (y : ys)) = let env = go (y :| ys) in eval env x `NonEmpty.cons` env

-- ⟦ x ↦ ξ.y, y ↦ ξ.z, z ↦ ⟦ a ↦ ⟦⟧ ⟧ ⟧.x
delayedYegorInsideOut :: Context -> Object -> Object
delayedYegorInsideOut ctx = quote . eval (evalContext ctx)

delayedYegorInsideOutAsRule :: NamedRule
delayedYegorInsideOutAsRule = ("Yegor's rules (hardcoded, with delayed substitutions)", \ctx obj -> [delayedYegorInsideOut ctx obj])

-- evaluate object into closure
eval :: Env -> Object -> ObjectClosure
eval env obj' = case obj' of
  Formation bindings -> FormationC env bindings []
  Application obj args -> applicationClosure (eval env obj) (map (evalBinding env) args)
  ObjectDispatch obj attr -> objectDispatchClosure (eval env obj) attr
  GlobalObject -> NonEmpty.last env
  ThisObject -> NonEmpty.head env
  Termination -> TerminationC
  MetaSubstThis{} -> error "cannot evaluate an object with meta subst"
  MetaObject{} -> error "cannot evaluate an object with metavariables"
  MetaFunction{} -> error "cannot evaluate an object with metafunctions"

evalBinding :: Env -> Binding -> BindingClosure
evalBinding env = \case
  AlphaBinding attr obj -> AlphaBindingC attr (eval env obj)
  DeltaBinding bytes -> DeltaBindingC bytes
  _ -> error "cannot evaluate empty of meta bindings"

envToRho :: Env -> Object
envToRho (_global :| []) = GlobalObject
envToRho (obj :| _env) = quote obj

-- recover object from closure
quote :: ObjectClosure -> Object
quote = \case
  FormationC env bindings bindingClosures ->
    Formation $
      concat
        [ bindings
        , map quoteBinding bindingClosures
        , [AlphaBinding Rho (envToRho env)]
        ]
  ObjectDispatchC objC attr -> ObjectDispatch (quote objC) attr
  ApplicationC obj args -> Application (quote obj) (map quoteBinding args)
  TerminationC -> Termination

quoteBinding :: BindingClosure -> Binding
quoteBinding = \case
  AlphaBindingC attr objC -> AlphaBinding attr (quote objC)
  DeltaBindingC bytes -> DeltaBinding bytes

objectDispatchClosure :: ObjectClosure -> Attribute -> ObjectClosure
objectDispatchClosure = objectDispatchClosure' []

objectDispatchClosure' :: [(ObjectClosure, Attribute)] -> ObjectClosure -> Attribute -> ObjectClosure
objectDispatchClosure' visited objC a
  | (objC, a) `elem` visited = TerminationC
  | otherwise =
      case objC of
        FormationC env bindings bindingClosures ->
          case a of
            Rho -> env NonEmpty.!! 1
            _ -> do
              let bindings' = map (evalBinding (objC' `NonEmpty.cons` env)) bindings
                  bindingClosures' = bindingClosures <> bindings'
                  objC' = FormationC env [] bindingClosures'
              case lookupBindingClosure a bindingClosures' of
                Just objA -> objA
                Nothing ->
                  case lookupBindingClosure Phi bindingClosures' of
                    Just objPhi -> objectDispatchClosure' ((objC, a) : visited) objPhi a
                    Nothing
                      | not (any isLambdaBinding bindings) -> TerminationC
                      | otherwise -> ObjectDispatchC objC a
        _ -> ObjectDispatchC objC a

splitPositionalArgs :: [BindingClosure] -> ([ObjectClosure], [BindingClosure])
splitPositionalArgs = go
 where
  go [] = ([], [])
  go (AlphaBindingC (Alpha _) obj : args) = (obj : objs, others)
   where
    (objs, others) = go args
  go (bindingC : args) = (objs, bindingC : others)
   where
    (objs, others) = go args

applicationBindingsClosure :: Env -> [BindingClosure] -> ([Binding], [BindingClosure]) -> ObjectClosure
applicationBindingsClosure env args (bindings, bindingClosures)
  | length args > length emptyBindings
      || length leftoverEmptyBindings + length args > length emptyBindings =
      TerminationC
  | otherwise =
      FormationC env attachedBindings (zipWith f emptyBindings positionalArgs ++ otherArgs ++ bindingClosures)
 where
  f (EmptyBinding a) = AlphaBindingC a
  f DeltaEmptyBinding = error "impossible: Δ-binding is used as a positional argument"
  f _ = error "impossible: not an empty (non-Δ) binding!"

  (positionalArgs, otherArgs) = splitPositionalArgs args
  (emptyBindings, attachedBindings) = partition isEmptyBinding bindings
  leftoverEmptyBindings = filter doesNotHaveArg (drop (length positionalArgs) emptyBindings)
  doesNotHaveArg (EmptyBinding a) =
    a
      `notElem` [argAttr | AlphaBindingC argAttr _ <- otherArgs]
  doesNotHaveArg DeltaEmptyBinding =
    null
      [d | d@(DeltaBindingC _) <- otherArgs]
  doesNotHaveArg _ = error "impossible: not an empty (non-Δ) binding"

applicationClosure :: ObjectClosure -> [BindingClosure] -> ObjectClosure
applicationClosure objC args =
  case objC of
    FormationC env bindings bindingClosures -> applicationBindingsClosure env args (bindings, bindingClosures)
    _ -> ApplicationC objC args

isLambdaBinding :: Binding -> Bool
isLambdaBinding LambdaBinding{} = True
isLambdaBinding _ = False

lookupBindingClosure :: Attribute -> [BindingClosure] -> Maybe ObjectClosure
lookupBindingClosure a (binding : bindingsC)
  | AlphaBindingC a' objC <- binding, a == a' = Just objC
  | otherwise = lookupBindingClosure a bindingsC
lookupBindingClosure _ [] = Nothing
