{-# LANGUAGE TypeApplications #-}

module Language.EO.Phi.Dependencies where

import Language.EO.Phi

import Control.Monad (foldM)

bindingAttr :: Binding -> Maybe Attribute
bindingAttr (AlphaBinding a _) = Just a
bindingAttr (EmptyBinding a) = Just a
bindingAttr (DeltaBinding _) = Just (Alpha (AlphaIndex "Δ"))
bindingAttr DeltaEmptyBinding = Just (Alpha (AlphaIndex "Δ"))
bindingAttr LambdaBinding{} = Just (Alpha (AlphaIndex "λ"))
bindingAttr MetaBindings{} = Nothing
bindingAttr MetaDeltaBinding{} = Nothing

zipBindings :: [Binding] -> [Binding] -> ([Binding], [(Binding, Binding)])
zipBindings xs ys = (xs' <> ys', collisions)
 where
  as = map bindingAttr xs
  bs = map bindingAttr ys

  xs' = [x | x <- xs, bindingAttr x `notElem` bs]
  ys' = [y | y <- ys, bindingAttr y `notElem` as]
  collisions =
    [ (x, y)
    | x <- xs
    , y <- ys
    , bindingAttr x == bindingAttr y
    ]

isPackage :: [Binding] -> Bool
isPackage = any isPackageBinding

isPackageBinding :: Binding -> Bool
isPackageBinding (LambdaBinding (Function "Package")) = True
isPackageBinding _ = False

mergeBinding :: Binding -> Binding -> Either String Binding
mergeBinding (AlphaBinding a (Formation xs)) (AlphaBinding b (Formation ys))
  | a == b = AlphaBinding a . Formation <$> mergeBindings xs ys
mergeBinding x y | x == y = return x
mergeBinding x y =
  Left $
    concat @[]
      [ "conflict when adding dependencies (trying to merge non-formations)"
      , printTree x
      , printTree y
      ]

mergeBindings :: [Binding] -> [Binding] -> Either String [Binding]
mergeBindings xs ys
  | isPackage xs && isPackage ys = do
      case zipBindings xs ys of
        (zs, collisions) -> do
          ws <- mapM (uncurry mergeBinding) collisions
          return (zs <> ws)
  | otherwise =
      Left $
        concat @[]
          [ "conflict when adding dependencies (trying to merge non-Package formations "
          , printTree (Formation xs)
          , printTree (Formation ys)
          , " )"
          ]

deepMerge :: Program -> Program -> Either String Program
deepMerge (Program xs) (Program ys) = Program <$> mergeBindings (mkPackage xs) (mkPackage ys)
 where
  mkPackage bs
    | isPackage bs = bs
    -- FIXME: check if lambda attribute exists and throw error!
    | otherwise = LambdaBinding (Function "Package") : bs

deepMergePrograms :: [Program] -> Either String Program
deepMergePrograms [] = Right (Program [])
deepMergePrograms (p : ps) = foldM deepMerge p ps
