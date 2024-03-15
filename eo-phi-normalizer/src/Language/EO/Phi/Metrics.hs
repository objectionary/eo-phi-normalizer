{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.EO.Phi.Metrics where

import Control.Lens ((+=))
import Control.Monad (forM_)
import Control.Monad.State (State, execState, runState)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Generics.Labels ()
import Data.Traversable (forM)
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

instance Semigroup Metrics where
  (<>) :: Metrics -> Metrics -> Metrics
  x <> y =
    Metrics
      { dataless = x.dataless + y.dataless
      , applications = x.applications + y.applications
      , formations = x.formations + y.formations
      , dispatches = x.dispatches + y.dispatches
      }

instance Monoid Metrics where
  mempty :: Metrics
  mempty = defaultMetrics

data BindingMetrics = BindingMetrics
  { name :: String
  , metrics :: Metrics
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

count :: (a -> Bool) -> [a] -> Int
count x = length . filter x

getHeight :: [Binding] -> [Int] -> Int
getHeight bindings heights
  | hasDeltaBinding = 1
  | otherwise = heightAttributes
 where
  heightAttributes =
    case heights of
      [] -> 0
      _ -> minimum heights + 1
  hasDeltaBinding = not $ null [undefined | DeltaBinding _ <- bindings]

countDataless :: Int -> Int
countDataless x
  | x == 0 || x > 2 = 1
  | otherwise = 0

class Inspectable a where
  inspect :: a -> State Metrics Int

instance Inspectable Binding where
  inspect :: Binding -> State Metrics Int
  inspect = \case
    AlphaBinding _ obj -> do
      inspect obj
    _ -> pure 0

instance Inspectable Object where
  inspect :: Object -> State Metrics Int
  inspect = \case
    Formation bindings -> do
      #formations += 1
      heights <- forM bindings inspect
      let height = getHeight bindings heights
      #dataless += countDataless height
      pure height
    Application obj bindings -> do
      #applications += 1
      _ <- inspect obj
      forM_ bindings inspect
      pure 0
    ObjectDispatch obj _ -> do
      #dispatches += 1
      _ <- inspect obj
      pure 0
    _ -> pure 0

type Path = [String]

data BindingsByPathMetrics = BindingsByPathMetrics
  { path :: Path
  , bindingsMetrics :: [BindingMetrics]
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

data ObjectMetrics = ObjectMetrics
  { bindingsByPathMetrics :: BindingsByPathMetrics
  , thisObjectMetrics :: Metrics
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

-- | Get metrics for an object
--
-- >>> getThisObjectMetrics "⟦ α0 ↦ ξ, α0 ↦ Φ.org.eolang.bytes( Δ ⤍ 00- ) ⟧"
-- Metrics {dataless = 0, applications = 1, formations = 1, dispatches = 3}
--
-- >>> getThisObjectMetrics "⟦ α0 ↦ ξ, Δ ⤍ 00- ⟧"
-- Metrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}
--
-- >>> getThisObjectMetrics "⟦ α0 ↦ ξ, α1 ↦ ⟦ Δ ⤍ 00- ⟧ ⟧"
-- Metrics {dataless = 0, applications = 0, formations = 2, dispatches = 0}
--
-- >>> getThisObjectMetrics "⟦ α0 ↦ ξ, α1 ↦ ⟦ α2 ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧"
-- Metrics {dataless = 0, applications = 0, formations = 3, dispatches = 0}
--
-- >>> getThisObjectMetrics "⟦ Δ ⤍ 00- ⟧"
-- Metrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}
--
-- >>> getThisObjectMetrics "⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧"
-- Metrics {dataless = 0, applications = 0, formations = 2, dispatches = 0}
--
-- >>> getThisObjectMetrics "⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧ ⟧"
-- Metrics {dataless = 1, applications = 0, formations = 3, dispatches = 0}
--
-- >>> getThisObjectMetrics "⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧ ⟧ ⟧"
-- Metrics {dataless = 2, applications = 0, formations = 4, dispatches = 0}
--
-- >>> getThisObjectMetrics "⟦ org ↦ ⟦ ⟧ ⟧"
-- Metrics {dataless = 1, applications = 0, formations = 2, dispatches = 0}
--
-- >>> getThisObjectMetrics "⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧.e ⟧"
-- Metrics {dataless = 1, applications = 1, formations = 5, dispatches = 5}
getThisObjectMetrics :: Object -> Metrics
getThisObjectMetrics obj = execState (inspect obj) mempty

-- | Get an object by a path within a given object.
--
-- If no object is accessible by the path, return a prefix of the path that led to a non-formation when the remaining path wasn't empty.
-- >>> getObjectByPath ["org", "eolang"] "⟦ org ↦ ⟦ eolang ↦ ⟦ x ↦ ⟦ φ ↦ Φ.org.eolang.bool ( α0 ↦ Φ.org.eolang.bytes (Δ ⤍ 01-) ) ⟧, z ↦ ⟦ y ↦ ⟦ x ↦ ∅, φ ↦ ξ.x ⟧, φ ↦ Φ.org.eolang.bool ( α0 ↦ Φ.org.eolang.bytes (Δ ⤍ 01-) ) ⟧, λ ⤍ Package ⟧, λ ⤍ Package ⟧⟧"
-- Right (Formation [AlphaBinding (Label (LabelId "x")) (Formation [AlphaBinding Phi (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bool"))) [AlphaBinding (Alpha (AlphaIndex "\945\&0")) (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bytes"))) [DeltaBinding (Bytes "01-")])])]),AlphaBinding (Label (LabelId "z")) (Formation [AlphaBinding (Label (LabelId "y")) (Formation [EmptyBinding (Label (LabelId "x")),AlphaBinding Phi (ObjectDispatch ThisObject (Label (LabelId "x")))]),AlphaBinding Phi (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bool"))) [AlphaBinding (Alpha (AlphaIndex "\945\&0")) (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bytes"))) [DeltaBinding (Bytes "01-")])])]),LambdaBinding (Function "Package")])
--
-- >>> getObjectByPath ["a"] "⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧.e ⟧"
-- Right (ObjectDispatch (Formation [AlphaBinding (Label (LabelId "b")) (Formation [EmptyBinding (Label (LabelId "c")),AlphaBinding (Label (LabelId "d")) (Formation [AlphaBinding Phi (ObjectDispatch (ObjectDispatch ThisObject Rho) (Label (LabelId "c")))])]),AlphaBinding (Label (LabelId "e")) (ObjectDispatch (Application (ObjectDispatch ThisObject (Label (LabelId "b"))) [AlphaBinding (Label (LabelId "c")) (Formation [])]) (Label (LabelId "d")))]) (Label (LabelId "e")))
getObjectByPath :: Path -> Object -> Either Path Object
getObjectByPath path object =
  case path of
    [] -> Right object
    (p : ps) ->
      case object of
        Formation bindings ->
          case objectByPath' of
            [] -> Left path
            (x : _) -> Right x
         where
          objectByPath' =
            do
              x <- bindings
              Right obj <-
                case x of
                  AlphaBinding (Alpha (AlphaIndex name)) obj | name == p -> [getObjectByPath ps obj]
                  AlphaBinding (Label (LabelId name)) obj | name == p -> [getObjectByPath ps obj]
                  _ -> [Left path]
              pure obj
        _ -> Left path

-- | Get metrics for bindings of a formation that is accessible by a path within a given object.
--
-- If no formation is accessible by the path, return a prefix of the path that led to a non-formation when the remaining path wasn't empty.
-- >>> getBindingsByPathMetrics ["a"] "⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧.e ⟧"
-- Left ["a"]
--
-- >>> getBindingsByPathMetrics ["a"] "⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧ ⟧"
-- Right (BindingsByPathMetrics {path = ["a"], bindingsMetrics = [BindingMetrics {name = "b", metrics = Metrics {dataless = 0, applications = 0, formations = 2, dispatches = 2}},BindingMetrics {name = "e", metrics = Metrics {dataless = 1, applications = 1, formations = 1, dispatches = 2}}]})
getBindingsByPathMetrics :: Path -> Object -> Either Path BindingsByPathMetrics
getBindingsByPathMetrics path obj =
  case getObjectByPath path obj of
    Right (Formation bindings) ->
      let attributes' = flip runState mempty . inspect <$> bindings
          (_, objectMetrics) = unzip attributes'
          bindingsMetrics = do
            x <- zip bindings objectMetrics
            case x of
              (AlphaBinding (Alpha (AlphaIndex name)) _, metrics) -> [BindingMetrics{..}]
              (AlphaBinding (Label (LabelId name)) _, metrics) -> [BindingMetrics{..}]
              _ -> []
       in Right $ BindingsByPathMetrics{..}
    Right _ -> Left path
    Left path' -> Left path'

-- | Get metrics for an object and for bindings of a formation accessible by a given path.
--
-- Combine metrics produced by 'getThisObjectMetrics' and 'getBindingsByPathMetrics'.
--
-- If no formation is accessible by the path, return a prefix of the path that led to a non-formation when the remaining path wasn't empty.
-- >>> getObjectMetrics ["a"] "⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧.e ⟧"
-- Left ["a"]
--
-- >>> getObjectMetrics ["a"] "⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧ ⟧"
-- Right (ObjectMetrics {bindingsByPathMetrics = BindingsByPathMetrics {path = ["a"], bindingsMetrics = [BindingMetrics {name = "b", metrics = Metrics {dataless = 0, applications = 0, formations = 2, dispatches = 2}},BindingMetrics {name = "e", metrics = Metrics {dataless = 1, applications = 1, formations = 1, dispatches = 2}}]}, thisObjectMetrics = Metrics {dataless = 1, applications = 1, formations = 5, dispatches = 4}})
getObjectMetrics :: Path -> Object -> Either Path ObjectMetrics
getObjectMetrics path obj = do
  let thisObjectMetrics = getThisObjectMetrics obj
  bindingsByPathMetrics <- getBindingsByPathMetrics path obj
  pure ObjectMetrics{..}

data ProgramMetrics = ProgramMetrics
  { bindingsByPathMetrics :: BindingsByPathMetrics
  , programMetrics :: Metrics
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

-- | Get metrics for a program and for bindings of a formation accessible by a given path.
--
-- Combine metrics produced by 'getThisObjectMetrics' and 'getBindingsByPathMetrics'.
--
-- If no formation is accessible by the path, return a prefix of the path that led to a non-formation when the remaining path wasn't empty.
-- >>> getProgramMetrics ["org", "eolang"] "{⟦ org ↦ ⟦ eolang ↦ ⟦ x ↦ ⟦ φ ↦ Φ.org.eolang.bool ( α0 ↦ Φ.org.eolang.bytes (Δ ⤍ 01-) ) ⟧, z ↦ ⟦ y ↦ ⟦ x ↦ ∅, φ ↦ ξ.x ⟧, φ ↦ Φ.org.eolang.bool ( α0 ↦ Φ.org.eolang.bytes (Δ ⤍ 01-) ) ⟧, λ ⤍ Package ⟧, λ ⤍ Package ⟧⟧ }"
-- Right (Formation [AlphaBinding (Label (LabelId "x")) (Formation [AlphaBinding Phi (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bool"))) [AlphaBinding (Alpha (AlphaIndex "\945\&0")) (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bytes"))) [DeltaBinding (Bytes "01-")])])]),AlphaBinding (Label (LabelId "z")) (Formation [AlphaBinding (Label (LabelId "y")) (Formation [EmptyBinding (Label (LabelId "x")),AlphaBinding Phi (ObjectDispatch ThisObject (Label (LabelId "x")))]),AlphaBinding Phi (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bool"))) [AlphaBinding (Alpha (AlphaIndex "\945\&0")) (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bytes"))) [DeltaBinding (Bytes "01-")])])]),LambdaBinding (Function "Package")])
--
-- >>> getProgramMetrics ["a"] "{⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧.e ⟧}"
-- Right (ObjectDispatch (Formation [AlphaBinding (Label (LabelId "b")) (Formation [EmptyBinding (Label (LabelId "c")),AlphaBinding (Label (LabelId "d")) (Formation [AlphaBinding Phi (ObjectDispatch (ObjectDispatch ThisObject Rho) (Label (LabelId "c")))])]),AlphaBinding (Label (LabelId "e")) (ObjectDispatch (Application (ObjectDispatch ThisObject (Label (LabelId "b"))) [AlphaBinding (Label (LabelId "c")) (Formation [])]) (Label (LabelId "d")))]) (Label (LabelId "e")))
getProgramMetrics :: Path -> Program -> Either Path ProgramMetrics
getProgramMetrics path (Program bindings) = do
  ObjectMetrics{..} <- getObjectMetrics path (Formation bindings)
  pure ProgramMetrics{programMetrics = thisObjectMetrics, ..}
