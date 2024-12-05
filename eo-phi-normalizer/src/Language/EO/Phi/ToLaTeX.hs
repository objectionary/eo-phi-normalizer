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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.ToLaTeX where

import Data.Foldable (fold)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Text qualified as T
import Language.EO.Phi
import Language.EO.Phi.Rules.Yaml
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
  toLatex (Alpha (AlphaIndex a)) = LaTeX ("\\alpha_" ++ tail a)
  toLatex (Label (LabelId l)) = LaTeX l
  toLatex (MetaAttr (LabelMetaId l)) = LaTeX l

instance ToLatex Binding where
  toLatex (AlphaBinding attr obj) = toLatex attr <> " -> " <> toLatex obj
  toLatex (EmptyBinding attr) = toLatex attr <> " -> ?"
  toLatex (DeltaBinding (Bytes bytes)) = "D> " <> LaTeX bytes
  toLatex DeltaEmptyBinding = "D> ?"
  toLatex (LambdaBinding (Function fn)) = "L> " <> LaTeX fn
  toLatex (MetaBindings (BindingsMetaId x)) = LaTeX x
  toLatex (MetaDeltaBinding (BytesMetaId x)) = "D> " <> LaTeX x

instance ToLatex Object where
  toLatex (Formation bindings) =
    "[[ " <> fold (intersperse ", " (map toLatex bindings)) <> " ]]"
  toLatex (Application obj bindings) =
    toLatex obj <> "( " <> fold (intersperse ", " (map toLatex bindings)) <> " )"
  toLatex (ObjectDispatch obj attr) =
    toLatex obj <> "." <> toLatex attr
  toLatex GlobalObject = "Q"
  toLatex ThisObject = "\\xi"
  toLatex Termination = "\\dead"
  toLatex (MetaObject (ObjectMetaId metaId)) = LaTeX metaId
  toLatex MetaTailContext{} = error "rendering MetaTailContext in LaTex format"
  toLatex (MetaFunction _ _) = error "rendering MetaFunction in LaTex format"
  toLatex (MetaSubstThis obj1 obj2) = LaTeX "\\mathbb{S}(" <> toLatex obj1 <> ", " <> toLatex obj2 <> ")"
  toLatex (MetaContextualize obj1 obj2) = LaTeX "\\lceil" <> toLatex obj1 <> ", " <> toLatex obj2 <> "\\rceil"
  toLatex (ConstString string) = "|" <> LaTeX (show string) <> "|"
  toLatex (ConstInt n) = LaTeX (show n)
  toLatex (ConstFloat x) = LaTeX (show x)

removeOrgEolang :: String -> String
removeOrgEolang = T.unpack . T.replace "Q.org.eolang" "QQ" . T.pack

substituteTau :: String -> String
substituteTau = T.unpack . T.replace "τ" "\\tau" . T.pack

removeExclamationsMarks :: String -> String
removeExclamationsMarks = filter (/= '!')

removeAlpha :: String -> String
removeAlpha s = subRegex (mkRegex "\\\\alpha_([0-9]+) ->") s "\\1->"

-- >>> toLatex ("{ ⟦ α0 ↦ ξ, α1 ↦ Φ.org.eolang.bytes( Δ ⤍ 00- ) ⟧ }" :: Program)
-- \Big\{ [[ 0-> $, 1-> QQ.bytes( D> 00- ) ]] \Big\}
latexToString :: LaTeX -> String
latexToString = removeExclamationsMarks . substituteTau . removeAlpha . removeOrgEolang . unLaTeX

inMathMode :: LaTeX -> LaTeX
inMathMode = (" $ " <>) . (<> " $ ")

-- it's ok without separators here because rules have zero or one constraint from the context
instance ToLatex RuleContext where
  toLatex RuleContext{..} =
    maybe mempty (\x -> inMathMode $ toLatex GlobalObject <> " -> " <> toLatex x) global_object
      <> maybe mempty (\x -> inMathMode (toLatex x) <> " is the scope of the redex") current_object
      <> maybe mempty (\x -> toLatex x <> " is the current attribute") current_attribute

instance ToLatex RuleAttribute where
  toLatex (ObjectAttr a) = toLatex a
  toLatex DeltaAttr = "\\Delta"
  toLatex LambdaAttr = "\\lambda"

instance ToLatex Condition where
  toLatex (IsNF nf) = inMathMode $ toLatex nf <> "\\in\\mathcal{N}"
  toLatex (IsNFInsideFormation nf_inside_formation) =
    inMathMode (toLatex nf_inside_formation) <> " is nf inside formation"
  toLatex (PresentAttrs (AttrsInBindings attrs bindings)) =
    inMathMode $ fold (intersperse ", " (map toLatex attrs)) <> " \\in " <> foldMap toLatex bindings
  toLatex (AbsentAttrs (AttrsInBindings attrs bindings)) =
    inMathMode $ fold (intersperse ", " (map toLatex attrs)) <> " \\notin " <> foldMap toLatex bindings
  toLatex (AttrNotEqual (attr1, attr2)) =
    inMathMode $ toLatex attr1 <> " \\neq " <> toLatex attr2
  toLatex (ApplyInSubformations apply_in_subformations) =
    if not apply_in_subformations then LaTeX "not in subformations" else mempty
  toLatex (ApplyInAbstractSubformations apply_in_abstract_subformations) =
    if not apply_in_abstract_subformations then LaTeX "not in subformations" else mempty

isNonEmptyContext :: Maybe RuleContext -> Bool
isNonEmptyContext Nothing = False
isNonEmptyContext (Just (RuleContext Nothing Nothing Nothing)) = False
isNonEmptyContext _ = True

-- Renders all conditions on separate lines
instance ToLatex Rule where
  toLatex (Rule name _ context _ pattern result _ when _) =
    "\\rrule{"
      <> LaTeX name
      <> "}: &"
      <> inMathMode (toLatex pattern)
      <> "\\(\\trans\\)"
      <> inMathMode (toLatex result)
      <> (if not (null when) || isNonEmptyContext context then "\\\\\\text {if }" else mempty)
      <> maybe mempty (\c -> "&" <> toLatex c <> "\\\\") context
      <> fold (intersperse ",\\\\" (maybe [] (map (("&" <>) . toLatex)) when))

instance ToLatex [Rule] where
  toLatex rules =
    "\\begin{figure*}\n\\begin{tabular}{rl}\n  "
      <> fold (intersperse "\\\\\\\\\n  " (map toLatex rules))
      <> "\n\\end{tabular}\n\\end{figure*}"

-- Renders all conditions in one line
ruleToLatexCompact :: Rule -> LaTeX
ruleToLatexCompact (Rule name _ context _ pattern result _ when _) =
  "\\rrule{"
    <> LaTeX name
    <> "}: "
    <> inMathMode (toLatex pattern)
    <> "\\(\\trans\\)"
    <> inMathMode (toLatex result)
    <> (if not (null when) || isNonEmptyContext context then "\\quad\\text {if }" else "")
    <> maybe mempty (\c -> toLatex c <> ", ") context
    <> fold (intersperse ", " (maybe [] (map toLatex) when))

rulesToLatexCompact :: [Rule] -> LaTeX
rulesToLatexCompact rules =
  "\\begin{figure*}\n  "
    <> fold (intersperse "\\\\\\vspace*{0.5em}\n  " (map ruleToLatexCompact rules))
    <> "\n\\end{figure*}"
