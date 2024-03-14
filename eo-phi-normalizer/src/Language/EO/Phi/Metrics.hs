{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.Metrics where

import Control.Lens ((+=))
import Control.Monad (forM_)
import Control.Monad.State (State, execState, runState)
import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Traversable (forM)
import GHC.Generics (Generic)
import Language.EO.Phi.Rules.Common ()
import Language.EO.Phi.Syntax.Abs

data ObjectMetrics = ObjectMetrics
  { dataless :: Int
  , applications :: Int
  , formations :: Int
  , dispatches :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, Eq)

defaultObjectMetrics :: ObjectMetrics
defaultObjectMetrics =
  ObjectMetrics
    { dataless = 0
    , applications = 0
    , formations = 0
    , dispatches = 0
    }

instance Semigroup ObjectMetrics where
  (<>) :: ObjectMetrics -> ObjectMetrics -> ObjectMetrics
  x <> y =
    ObjectMetrics
      { dataless = x.dataless + y.dataless
      , applications = x.applications + y.applications
      , formations = x.formations + y.formations
      , dispatches = x.dispatches + y.dispatches
      }

instance Monoid ObjectMetrics where
  mempty :: ObjectMetrics
  mempty = defaultObjectMetrics

data BindingMetrics = BindingMetrics
  { name :: String
  , metrics :: ObjectMetrics
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
  inspect :: a -> State ObjectMetrics Int

instance Inspectable Binding where
  inspect :: Binding -> State ObjectMetrics Int
  inspect = \case
    AlphaBinding _ obj -> do
      inspect obj
    _ -> pure 0

instance Inspectable Object where
  inspect :: Object -> State ObjectMetrics Int
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

data CompleteMetrics = CompleteMetrics
  { attributes :: [BindingMetrics]
  , this :: ObjectMetrics
  }
  deriving (Show, Generic, FromJSON, ToJSON, Eq)

defaultMetrics :: CompleteMetrics
defaultMetrics =
  CompleteMetrics
    { attributes = []
    , this = defaultObjectMetrics
    }

-- | Collect metrics for an object
--
-- When an object is a formation, provide metrics for its attributes and metrics for the object.
--
-- >>> collectCompleteMetrics "⟦ α0 ↦ ξ, α0 ↦ Φ.org.eolang.bytes( Δ ⤍ 00- ) ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 0, dispatches = 0}},BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 1, formations = 0, dispatches = 3}}], this = ObjectMetrics {dataless = 0, applications = 1, formations = 1, dispatches = 3}}
--
-- >>> collectCompleteMetrics "⟦ α0 ↦ ξ, Δ ⤍ 00- ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 0, dispatches = 0}}], this = ObjectMetrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}}
--
-- >>> collectCompleteMetrics "⟦ α0 ↦ ξ, α1 ↦ ⟦ Δ ⤍ 00- ⟧ ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 0, dispatches = 0}},BindingMetrics {name = "\945\&1", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}}], this = ObjectMetrics {dataless = 0, applications = 0, formations = 2, dispatches = 0}}
--
-- >>> collectCompleteMetrics "⟦ α0 ↦ ξ, α1 ↦ ⟦ α2 ↦ ⟦ Δ ⤍ 00- ⟧ ⟧ ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 0, dispatches = 0}},BindingMetrics {name = "\945\&1", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 2, dispatches = 0}}], this = ObjectMetrics {dataless = 0, applications = 0, formations = 3, dispatches = 0}}
--
-- >>> collectCompleteMetrics "⟦ Δ ⤍ 00- ⟧"
-- CompleteMetrics {attributes = [], this = ObjectMetrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}}
--
-- >>> collectCompleteMetrics "⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 1, dispatches = 0}}], this = ObjectMetrics {dataless = 0, applications = 0, formations = 2, dispatches = 0}}
--
-- >>> collectCompleteMetrics "⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧ ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 0, applications = 0, formations = 2, dispatches = 0}}], this = ObjectMetrics {dataless = 1, applications = 0, formations = 3, dispatches = 0}}
--
-- >>> collectCompleteMetrics "⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ⟦ α0 ↦ ∅ ⟧ ⟧ ⟧ ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "\945\&0", metrics = ObjectMetrics {dataless = 1, applications = 0, formations = 3, dispatches = 0}}], this = ObjectMetrics {dataless = 2, applications = 0, formations = 4, dispatches = 0}}
--
-- >>> collectCompleteMetrics "⟦ org ↦ ⟦ ⟧ ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "org", metrics = ObjectMetrics {dataless = 1, applications = 0, formations = 1, dispatches = 0}}], this = ObjectMetrics {dataless = 1, applications = 0, formations = 2, dispatches = 0}}
--
-- >>> collectCompleteMetrics "⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧.e ⟧"
-- CompleteMetrics {attributes = [BindingMetrics {name = "a", metrics = ObjectMetrics {dataless = 1, applications = 1, formations = 4, dispatches = 5}}], this = ObjectMetrics {dataless = 1, applications = 1, formations = 5, dispatches = 5}}
collectCompleteMetrics :: Object -> CompleteMetrics
collectCompleteMetrics = \case
  Formation bindings ->
    let attributes' = flip runState mempty . inspect <$> bindings
        (heights, objectMetrics) = unzip attributes'
        attributes = do
          x <- zip bindings objectMetrics
          case x of
            (AlphaBinding (Alpha (AlphaIndex name)) _, metrics) -> [BindingMetrics{..}]
            (AlphaBinding (Label (LabelId name)) _, metrics) -> [BindingMetrics{..}]
            _ -> []
        height = getHeight bindings heights
        this =
          fold objectMetrics
            & \x ->
              x
                { dataless = x.dataless + countDataless height
                , formations = x.formations + 1
                }
     in CompleteMetrics{..}
  obj ->
    CompleteMetrics
      { attributes = []
      , this = execState (inspect obj) mempty
      }

objectByPath :: [String] -> Object -> Either [String] Object
objectByPath path object =
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
                  AlphaBinding (Alpha (AlphaIndex name)) obj | name == p -> [objectByPath ps obj]
                  AlphaBinding (Label (LabelId name)) obj | name == p -> [objectByPath ps obj]
                  _ -> [Left path]
              pure obj
        _ -> Left path

-- >>> programObjectByPath ["org", "eolang"] "{⟦ org ↦ ⟦ eolang ↦ ⟦ x ↦ ⟦ φ ↦ Φ.org.eolang.bool ( α0 ↦ Φ.org.eolang.bytes (Δ ⤍ 01-) ) ⟧, z ↦ ⟦ y ↦ ⟦ x ↦ ∅, φ ↦ ξ.x ⟧, φ ↦ Φ.org.eolang.bool ( α0 ↦ Φ.org.eolang.bytes (Δ ⤍ 01-) ) ⟧, λ ⤍ Package ⟧, λ ⤍ Package ⟧⟧ }"
-- Right (Formation [AlphaBinding (Label (LabelId "x")) (Formation [AlphaBinding Phi (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bool"))) [AlphaBinding (Alpha (AlphaIndex "\945\&0")) (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bytes"))) [DeltaBinding (Bytes "01-")])])]),AlphaBinding (Label (LabelId "z")) (Formation [AlphaBinding (Label (LabelId "y")) (Formation [EmptyBinding (Label (LabelId "x")),AlphaBinding Phi (ObjectDispatch ThisObject (Label (LabelId "x")))]),AlphaBinding Phi (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bool"))) [AlphaBinding (Alpha (AlphaIndex "\945\&0")) (Application (ObjectDispatch (ObjectDispatch (ObjectDispatch GlobalObject (Label (LabelId "org"))) (Label (LabelId "eolang"))) (Label (LabelId "bytes"))) [DeltaBinding (Bytes "01-")])])]),LambdaBinding (Function "Package")])
--
-- >>> programObjectByPath ["a"] "{⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧.e ⟧}"
-- Right (ObjectDispatch (Formation [AlphaBinding (Label (LabelId "b")) (Formation [EmptyBinding (Label (LabelId "c")),AlphaBinding (Label (LabelId "d")) (Formation [AlphaBinding Phi (ObjectDispatch (ObjectDispatch ThisObject Rho) (Label (LabelId "c")))])]),AlphaBinding (Label (LabelId "e")) (ObjectDispatch (Application (ObjectDispatch ThisObject (Label (LabelId "b"))) [AlphaBinding (Label (LabelId "c")) (Formation [])]) (Label (LabelId "d")))]) (Label (LabelId "e")))
programObjectByPath :: [String] -> Program -> Either [String] Object
programObjectByPath path (Program bindings) = objectByPath path (Formation bindings)
