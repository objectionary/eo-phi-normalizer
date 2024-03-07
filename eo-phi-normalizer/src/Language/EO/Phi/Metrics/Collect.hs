{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

{- FOURMOLU_DISABLE -}

-- $setup
-- >>> :set -XOverloadedStrings

{- FOURMOLU_ENABLE -}

module Language.EO.Phi.Metrics.Collect where

import Control.Lens ((+=))
import Control.Monad (forM_)
import Control.Monad.State (State, execState)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Language.EO.Phi.Rules.Common ()
import Language.EO.Phi.Syntax.Abs

data Metrics = Metrics
  { dataless :: Int
  , applications :: Int
  , formations :: Int
  , dispatches :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

defaultMetrics :: Metrics
defaultMetrics =
  Metrics
    { dataless = 0
    , applications = 0
    , formations = 0
    , dispatches = 0
    }

collectMetrics :: (Inspectable a) => a -> Metrics
collectMetrics a = execState (inspect a) defaultMetrics

class Inspectable a where
  inspect :: a -> State Metrics ()

count :: (a -> Bool) -> [a] -> Int
count x = length . filter x

-- | Count dataless formations in a list of bindings
--
-- >>> countDataless' :: Object -> Int; countDataless' x = let Formation bindings = x in countDataless bindings
--
-- >>> countDataless' "⟦ α0 ↦ ξ, α0 ↦ Φ.org.eolang.bytes( Δ ⤍ 00-00-00-00-00-00-00-2A ) ⟧"
-- 1
--
-- >>> countDataless' "⟦ α0 ↦ ξ, Δ ⤍ 00-00-00-00-00-00-00-2A ⟧"
-- 0
--
--
-- >>> countDataless' "⟦ α0 ↦ ξ, α1 ↦ ⟦ Δ ⤍ 00-00-00-00-00-00-00-2A ⟧ ⟧"
-- 0
--
--
-- >>> countDataless' "⟦ α0 ↦ ξ, α1 ↦ ⟦ α2 ↦ ⟦ Δ ⤍ 00-00-00-00-00-00-00-2A ⟧ ⟧ ⟧"
-- 1
countDataless :: (Num a) => [Binding] -> a
countDataless bindings =
  let countDeltas = count (\case DeltaBinding _ -> True; DeltaEmptyBinding -> True; _ -> False)
      nestedBindings = concatMap (\case AlphaBinding _ (Formation bindings') -> bindings'; _ -> []) bindings
      deltas = countDeltas (bindings <> nestedBindings)
   in if deltas == 0 then 1 else 0

instance Inspectable Program where
  inspect (Program bindings) = inspect (Formation bindings)

instance Inspectable Binding where
  inspect = \case
    AlphaBinding attr obj -> do
      inspect attr
      inspect obj
    EmptyBinding attr -> do
      inspect attr
    _ -> pure ()

instance Inspectable Attribute where
  inspect _ = pure ()

instance Inspectable Object where
  inspect = \case
    Formation bindings -> do
      #dataless += countDataless bindings
      #formations += 1
      forM_ bindings inspect
    Application obj bindings -> do
      #applications += 1
      inspect obj
      forM_ bindings inspect
    ObjectDispatch obj attr -> do
      #dispatches += 1
      inspect obj
      inspect attr
    _ -> pure ()
