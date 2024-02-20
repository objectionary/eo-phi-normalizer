{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Language.EO.Phi.Metrics.Collect where

import Control.Lens ((+=))
import Control.Monad (forM_, when)
import Control.Monad.State (MonadState, State, execState)
import Data.Aeson (FromJSON)
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
  deriving (Generic, Show, FromJSON, Eq)

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

countDataless :: (MonadState Metrics m) => [Binding] -> m ()
countDataless bindings = do
  let countDeltas = count (\case DeltaBinding _ -> True; _ -> False)
      deltas = countDeltas (bindings <> concatMap (\case AlphaBinding _ (Formation bindings') -> bindings'; _ -> []) bindings)
  when (deltas == 0) (#dataless += 1)

instance Inspectable Program where
  inspect (Program bindings) = do
    countDataless bindings
    forM_ bindings inspect

instance Inspectable Binding where
  inspect = \case
    AlphaBinding attr obj -> do
      inspect attr
      inspect obj
    EmptyBinding attr -> do
      inspect attr
    DeltaBinding _ -> pure ()
    LambdaBinding _ -> pure ()
    MetaBindings _ -> pure ()

instance Inspectable Attribute where
  inspect = \case
    Phi -> pure ()
    Rho -> pure ()
    Sigma -> pure ()
    VTX -> pure ()
    Label _ -> pure ()
    Alpha _ -> pure ()
    MetaAttr _ -> pure ()

instance Inspectable Object where
  inspect = \case
    Formation bindings -> do
      countDataless bindings
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
    GlobalObject -> pure ()
    ThisObject -> pure ()
    Termination -> pure ()
    MetaObject _ -> pure ()
    MetaFunction _ _ -> pure ()
