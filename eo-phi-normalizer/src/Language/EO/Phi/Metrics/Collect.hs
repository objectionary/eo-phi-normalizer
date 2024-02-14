{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Language.EO.Phi.Metrics.Collect where

import Control.Lens ((+=))
import Control.Monad (forM_)
import Control.Monad.State (MonadState)
import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Language.EO.Phi.Syntax.Abs

data Metrics = Metrics
    { dataless :: Int
    , applications :: Int
    , formations :: Int
    , dispatches :: Int
    }
    deriving (Generic)

class Inspectable a where
    inspect :: (MonadState Metrics m) => a -> m ()

instance Inspectable Program where
    inspect (Program binding) = forM_ binding inspect

instance Inspectable Binding where
    inspect = \case
        AlphaBinding attr obj -> do
            inspect attr
            inspect obj
        EmptyBinding attr -> do
            #dataless += 1
            inspect attr
        DeltaBinding _ -> pure ()
        LambdaBinding _ -> #dataless += 1
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
