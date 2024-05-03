{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.EO.Phi.ToLaTeX where

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Text qualified as T
import Language.EO.Phi
import Text.Regex (mkRegex, subRegex)

newtype LaTeX = LaTeX {unLaTeX :: String}
  deriving newtype (IsString, Semigroup, Monoid)

instance Show LaTeX where
  show = latexToString

class ToLatex a where
  toLatex :: a -> LaTeX
  toLatexString :: a -> String
  toLatexString = latexToString . toLatex

instance ToLatex Program where
  toLatex (Program bindings) =
    "\\Big\\{ " <> toLatex (Formation bindings) <> " \\Big\\}"

instance ToLatex Attribute where
  toLatex Phi = "@"
  toLatex Rho = "^"
  toLatex Sigma = "&"
  toLatex (Alpha (AlphaIndex a)) = LaTeX ("\\alpha_" ++ tail a)
  toLatex (Label (LabelId l)) = LaTeX l
  toLatex (MetaAttr _) = error "rendering MetaBindings in LaTex format"
  toLatex VTX = error "rendering VTX in LaTex format"

instance ToLatex Binding where
  toLatex (AlphaBinding attr obj) = toLatex attr <> " -> " <> toLatex obj
  toLatex (EmptyBinding attr) = toLatex attr <> " -> ?"
  toLatex (DeltaBinding (Bytes bytes)) = "D> " <> LaTeX bytes
  toLatex DeltaEmptyBinding = "D> ?"
  toLatex (LambdaBinding (Function fn)) = "L> " <> LaTeX fn
  toLatex (MetaBindings _) = error "rendering MetaBindings in LaTex format"
  toLatex (MetaDeltaBinding _) = error "rendering MetaDeltaBinding in LaTex format"

instance ToLatex Object where
  toLatex (Formation bindings) =
    "[[ " <> fold (intersperse ", " (map toLatex bindings)) <> " ]]"
  toLatex (Application obj bindings) =
    toLatex obj <> "( " <> fold (intersperse ", " (map toLatex bindings)) <> " )"
  toLatex (ObjectDispatch obj attr) =
    toLatex obj <> "." <> toLatex attr
  toLatex GlobalObject = "Q"
  toLatex ThisObject = "$"
  toLatex Termination = "\\dead"
  toLatex (MetaObject _) = error "rendering MetaObject in LaTex format"
  toLatex (MetaFunction _ _) = error "rendering MetaFunction in LaTex format"
  toLatex (MetaSubstThis _ _) = error "rendering MetaSubstThis in LaTex format"

removeOrgEolang :: String -> String
removeOrgEolang = T.unpack . T.replace "Q.org.eolang" "QQ" . T.pack

removeAlpha :: String -> String
removeAlpha s = subRegex (mkRegex "\\\\alpha_([0-9]+) ->") s "\\1->"

-- >>> toLatex ("{ ⟦ α0 ↦ ξ, α1 ↦ Φ.org.eolang.bytes( Δ ⤍ 00- ) ⟧ }" :: Program)
-- \Big\{ [[ 0-> $, 1-> QQ.bytes( D> 00- ) ]] \Big\}
latexToString :: LaTeX -> String
latexToString = removeAlpha . removeOrgEolang . unLaTeX
