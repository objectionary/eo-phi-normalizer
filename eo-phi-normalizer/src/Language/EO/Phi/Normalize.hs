{- FOURMOLU_DISABLE -}
-- The MIT License (MIT)

-- Copyright (c) 2016-2024 Objectionary.com

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{- FOURMOLU_ENABLE -}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Language.EO.Phi.Normalize (
  normalizeObject,
  normalize,
  peelObject,
  unpeelObject,
) where

import Control.Monad.State
import Data.Maybe (fromMaybe)

import Data.Generics.Labels ()
import GHC.Generics (Generic)
import Language.EO.Phi.Rules.Common (lookupBinding, objectBindings)
import Language.EO.Phi.Syntax (desugar)
import Language.EO.Phi.Syntax.Abs

data Context = Context
  { globalObject :: [Binding]
  , thisObject :: [Binding]
  }
  deriving (Generic)

-- | Normalize an input 𝜑-program.
normalize :: Program -> Program
normalize (Program bindings) = evalState (Program . objectBindings <$> normalizeObject (Formation bindings)) context
 where
  context =
    Context
      { globalObject = bindings
      , thisObject = bindings
      }

normalizeObject :: Object -> State Context Object
normalizeObject object = do
  this <- gets thisObject
  case object of
    ObjectDispatch ThisObject a -> pure $ fromMaybe object (lookupBinding a this)
    _ -> pure object

-- | Split compound object into its head and applications/dispatch actions.
peelObject :: Object -> PeeledObject
peelObject = \case
  Formation bindings -> PeeledObject (HeadFormation bindings) []
  Application object bindings -> peelObject object `followedBy` ActionApplication bindings
  ObjectDispatch object attr -> peelObject object `followedBy` ActionDispatch attr
  GlobalObject -> PeeledObject HeadGlobal []
  obj@GlobalObjectPhiOrg -> peelObject (desugar obj)
  ThisObject -> PeeledObject HeadThis []
  Termination -> PeeledObject HeadTermination []
  MetaObject _ -> PeeledObject HeadTermination []
  MetaTailContext{} -> error "impossible"
  MetaFunction _ _ -> error "To be honest, I'm not sure what should be here"
  MetaSubstThis{} -> error "impossible"
  MetaContextualize{} -> error "impossible"
  obj@ConstString{} -> peelObject (desugar obj)
  obj@ConstInt{} -> peelObject (desugar obj)
  obj@ConstIntRaw{} -> peelObject (desugar obj)
  obj@ConstFloat{} -> peelObject (desugar obj)
  obj@ConstFloatRaw{} -> peelObject (desugar obj)
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
