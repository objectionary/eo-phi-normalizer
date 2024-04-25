{-# LANGUAGE OverloadedStrings #-}

module Language.EO.Phi.ToLaTeX where

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Text qualified as T
import Language.EO.Phi (Binding (..), Object (..), Program (Program))
import Language.EO.Phi.Syntax.Abs (AlphaIndex (..), Attribute (..), Bytes (..), Function (..), LabelId (..))

newtype LaTeX = LaTeX String
  deriving (Show)

instance Semigroup LaTeX where
  (LaTeX x) <> (LaTeX y) = LaTeX (x <> y)

instance Monoid LaTeX where
  mempty = LaTeX ""
  mappend = (<>)

class ToLatex a where
  toLatex :: a -> LaTeX

instance ToLatex Program where
  toLatex (Program bindings) =
    LaTeX "\\Big\\{ " <> toLatex (Formation bindings) <> LaTeX " \\Big\\}"

instance ToLatex Attribute where
  toLatex Phi = LaTeX "@"
  toLatex Rho = LaTeX "^"
  toLatex Sigma = LaTeX "&"
  toLatex (Alpha (AlphaIndex a)) = LaTeX ("\\alpha_" ++ tail a)
  toLatex (Label (LabelId l)) = LaTeX l
  toLatex _ = undefined

instance ToLatex Binding where
  toLatex (AlphaBinding attr obj) =
    toLatex attr <> LaTeX " -> " <> toLatex obj
  toLatex (EmptyBinding attr) =
    toLatex attr <> LaTeX " -> ?"
  toLatex (DeltaBinding (Bytes bytes)) =
    LaTeX "D> " <> LaTeX bytes
  toLatex DeltaEmptyBinding =
    LaTeX "D> ?"
  toLatex (LambdaBinding (Function fn)) =
    LaTeX "L> " <> LaTeX fn
  toLatex _ = undefined

instance ToLatex Object where
  toLatex (Formation bindings) =
    LaTeX "[[ " <> fold (intersperse (LaTeX ", ") (map toLatex bindings)) <> LaTeX " ]]"
  toLatex (Application obj bindings) =
    toLatex obj <> LaTeX "( " <> fold (intersperse (LaTeX ", ") (map toLatex bindings)) <> LaTeX " )"
  toLatex (ObjectDispatch obj attr) =
    toLatex obj <> LaTeX "." <> toLatex attr
  toLatex GlobalObject = LaTeX "Q"
  toLatex ThisObject = LaTeX "$"
  toLatex Termination = undefined -- todo: ask Yegor
  toLatex _ = undefined

removeOrgEolang :: String -> String
removeOrgEolang = T.unpack . T.replace "Q.org.eolang" "QQ" . T.pack

latexToString :: LaTeX -> String
latexToString (LaTeX a) = a
