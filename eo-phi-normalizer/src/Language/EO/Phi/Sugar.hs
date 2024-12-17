{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.EO.Phi.Sugar (desugarUnsafe, sugarUnsafe, desugarSugared, Sugared, Desugarable(..), Sugarable(..)) where

newtype Sugared a = Sugared a

class Desugarable a where
  desugar :: Sugared a -> a

class Sugarable a where
  sugar :: a -> Sugared a

desugarUnsafe :: (Desugarable a) => Sugared a -> a
desugarUnsafe (Sugared a) = a

desugarSugared :: (Desugarable a) => a -> a
desugarSugared = desugar . sugarUnsafe

sugarUnsafe :: a -> Sugared a
sugarUnsafe = Sugared
