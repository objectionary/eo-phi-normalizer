{-# LANGUAGE LambdaCase #-}

module Language.EO.Phi.Normalize (
  normalizeObject,
  normalize,
  peelObject,
  unpeelObject,
) where

import Control.Monad.State
import Data.Maybe (fromMaybe)
import Numeric (showHex)

import Control.Monad (forM)
import Language.EO.Phi.Rules.Common (lookupBinding)
import Language.EO.Phi.Syntax.Abs

data Context = Context
  { globalObject :: [Binding]
  , thisObject :: [Binding]
  , totalNuCount :: Int
  }

isNu :: Binding -> Bool
isNu (AlphaBinding VTX _) = True
isNu (EmptyBinding VTX) = True
isNu _ = False

-- | Normalize an input 𝜑-program.
normalize :: Program -> Program
normalize (Program bindings) = evalState (Program . objectBindings <$> normalizeObject (Formation bindings)) context
 where
  context =
    Context
      { globalObject = bindings
      , thisObject = bindings
      , totalNuCount = nuCount bindings
      }
  nuCount binds = count isNu binds + sum (map (sum . map (nuCount . objectBindings) . values) binds)
  count = (length .) . filter
  values (AlphaBinding _ obj) = [obj]
  values _ = []
  objectBindings (Formation bs) = bs
  objectBindings _ = []

rule1 :: Object -> State Context Object
rule1 (Formation bindings) = do
  normalizedBindings <- forM bindings $ \case
    AlphaBinding a object
      | a /= VTX ->
          do
            object' <- rule1 object
            pure (AlphaBinding a object')
    b -> pure b
  finalBindings <-
    if not $ any isNu normalizedBindings
      then do
        nus <- gets totalNuCount
        modify (\c -> c{totalNuCount = totalNuCount c + 1})
        let pad s = (if even (length s) then "" else "0") ++ s
        let insertDashes s
              | length s <= 2 = s ++ "-"
              | otherwise =
                  let go = \case
                        [] -> []
                        [x] -> [x]
                        [x, y] -> [x, y, '-']
                        (x : y : xs) -> x : y : '-' : go xs
                   in go s
        let dataObject = Formation [DeltaBinding $ Bytes $ insertDashes $ pad $ showHex nus ""]
        pure (AlphaBinding VTX dataObject : normalizedBindings)
      else do
        pure normalizedBindings
  pure (Formation finalBindings)
rule1 object = pure object

normalizeObject :: Object -> State Context Object
normalizeObject object = do
  this <- gets thisObject
  case object of
    -- Rule 1
    obj@(Formation _) -> rule1 obj
    ObjectDispatch ThisObject a -> pure $ fromMaybe object (lookupBinding a this)
    _ -> pure object

-- | Split compound object into its head and applications/dispatch actions.
peelObject :: Object -> PeeledObject
peelObject = \case
  Formation bindings -> PeeledObject (HeadFormation bindings) []
  Application object bindings -> peelObject object `followedBy` ActionApplication bindings
  ObjectDispatch object attr -> peelObject object `followedBy` ActionDispatch attr
  GlobalObject -> PeeledObject HeadGlobal []
  ThisObject -> PeeledObject HeadThis []
  Termination -> PeeledObject HeadTermination []
  MetaObject _ -> PeeledObject HeadTermination []
 where
  followedBy (PeeledObject object actions) action = PeeledObject object (actions ++ [action])

unpeelObject :: PeeledObject -> Object
unpeelObject (PeeledObject head_ actions) =
  case head_ of
    HeadFormation bindings -> go (Formation bindings) actions
    HeadGlobal ->
      case actions of
        ActionApplication{} : _ -> error "impossible: application for a global object!"
        _ -> go GlobalObject actions
    HeadThis ->
      case actions of
        ActionApplication{} : _ -> error "impossible: application for a global object!"
        _ -> go ThisObject actions
    HeadTermination -> go Termination actions
 where
  go = foldl applyAction
  applyAction object = \case
    ActionDispatch attr -> ObjectDispatch object attr
    ActionApplication bindings -> Application object bindings
