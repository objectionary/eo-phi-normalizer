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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- {-# LANGUAGE OverloadedStrings #-}

module Language.EO.Phi.Pretty where

import Language.EO.Phi.Syntax.Abs as Abs
import Prettyprinter

instance Pretty Abs.Bytes where
  pretty (Abs.Bytes i) = pretty i

instance Pretty Abs.Function where
  pretty (Abs.Function i) = pretty i

instance Pretty Abs.LabelId where
  pretty (Abs.LabelId i) = pretty i

instance Pretty Abs.AlphaIndex where
  pretty (Abs.AlphaIndex i) = pretty i

instance Pretty Abs.LabelMetaId where
  pretty (Abs.LabelMetaId i) = pretty i

instance Pretty Abs.TailMetaId where
  pretty (Abs.TailMetaId i) = pretty i

instance Pretty Abs.BindingsMetaId where
  pretty (Abs.BindingsMetaId i) = pretty i

instance Pretty Abs.ObjectMetaId where
  pretty (Abs.ObjectMetaId i) = pretty i

instance Pretty Abs.BytesMetaId where
  pretty (Abs.BytesMetaId i) = pretty i

instance Pretty Abs.MetaFunctionName where
  pretty (Abs.MetaFunctionName i) = pretty i

instance Pretty Abs.IntegerSigned where
  pretty (Abs.IntegerSigned i) = pretty i

instance Pretty Abs.DoubleSigned where
  pretty (Abs.DoubleSigned i) = pretty i

instance Pretty Abs.Program where
  pretty = \case
    Abs.Program bindings ->
      vsep
        [ lbrace
        , indent 2 (pretty (Formation bindings))
        , rbrace
        ]

instance Pretty Abs.MetaId where
  pretty = \case
    Abs.MetaIdLabel labelmetaid -> pretty labelmetaid
    Abs.MetaIdTail tailmetaid -> pretty tailmetaid
    Abs.MetaIdBindings bindingsmetaid -> pretty bindingsmetaid
    Abs.MetaIdObject objectmetaid -> pretty objectmetaid
    Abs.MetaIdBytes bytesmetaid -> pretty bytesmetaid

instance Pretty Abs.Object where
  pretty = \case
    Abs.Formation bindings ->
      case bindings of
        [] -> pretty "⟦" <> pretty "⟧"
        _ -> vsep [pretty "⟦", indent 2 (pretty bindings), pretty "⟧"]
    Abs.Application object bindings ->
      case bindings of
        [] -> pretty object <> lparen <> rparen
        _ -> vsep [pretty object <> lparen, indent 2 (pretty bindings), rparen]
    Abs.ObjectDispatch object attribute -> pretty object <> pretty "." <> pretty attribute
    Abs.GlobalObject -> pretty "Φ"
    Abs.GlobalObjectPhiOrg -> pretty "Φ̇"
    Abs.ThisObject -> pretty "ξ"
    Abs.Termination -> pretty "⊥"
    Abs.ConstString str -> pretty (show str)
    Abs.ConstIntRaw integersigned -> pretty integersigned
    Abs.ConstFloatRaw doublesigned -> pretty doublesigned
    Abs.MetaSubstThis object1 object2 -> pretty object1 <+> lbracket <+> pretty "ξ ↦" <+> pretty object2 <+> rbracket
    Abs.MetaContextualize object1 object2 -> pretty "⌈" <+> pretty object1 <> pretty "," <+> pretty object2 <> pretty "⌉"
    Abs.MetaObject objectmetaid -> pretty objectmetaid
    Abs.MetaTailContext object tailmetaid -> pretty object <+> pretty "*" <+> pretty tailmetaid
    Abs.MetaFunction metafunctionname object -> vsep [pretty metafunctionname <+> lparen, pretty object, rparen]
    Abs.ConstFloat d -> pretty d
    Abs.ConstInt n -> pretty n

instance Pretty Abs.Binding where
  pretty = \case
    Abs.AlphaBinding attribute object -> pretty attribute <+> pretty "↦" <+> pretty object
    Abs.AlphaBindingSugar object -> pretty object
    Abs.EmptyBinding attribute -> pretty attribute <+> pretty "↦ ∅"
    Abs.DeltaBinding bytes -> pretty "Δ ⤍" <+> pretty bytes
    Abs.DeltaEmptyBinding -> pretty "Δ ⤍ ∅"
    Abs.LambdaBinding function -> pretty "λ ⤍" <+> pretty function
    Abs.MetaBindings bindingsmetaid -> pretty bindingsmetaid
    Abs.MetaDeltaBinding bytesmetaid -> pretty "Δ ⤍" <+> pretty bytesmetaid

instance {-# OVERLAPPING #-} Pretty [Abs.Binding] where
  pretty = vsep . punctuate comma . fmap pretty

instance Pretty Abs.Attribute where
  pretty = \case
    Abs.Phi -> pretty "φ"
    Abs.Rho -> pretty "ρ"
    Abs.Label labelid -> pretty labelid
    Abs.Alpha alphaindex -> pretty alphaindex
    Abs.MetaAttr labelmetaid -> pretty labelmetaid
    Abs.AttrSugar labelid labelids -> pretty labelid <> lparen <> pretty labelids <> rparen

instance {-# OVERLAPPING #-} Pretty [Abs.LabelId] where
  pretty = hsep . punctuate comma . fmap pretty

instance Pretty Abs.RuleAttribute where
  pretty = \case
    Abs.ObjectAttr attribute -> pretty attribute
    Abs.DeltaAttr -> pretty "Δ"
    Abs.LambdaAttr -> pretty "λ"

instance Pretty Abs.PeeledObject where
  pretty = \case
    Abs.PeeledObject objecthead objectactions -> pretty objecthead <+> pretty objectactions

instance Pretty Abs.ObjectHead where
  pretty = \case
    Abs.HeadFormation bindings -> vsep [pretty "⟦", indent 2 (pretty bindings), pretty "⟧"]
    Abs.HeadGlobal -> pretty "Φ"
    Abs.HeadThis -> pretty "ξ"
    Abs.HeadTermination -> pretty "⊥"

instance Pretty Abs.ObjectAction where
  pretty = \case
    Abs.ActionApplication bindings -> vsep [lparen, indent 2 (pretty bindings), rparen]
    Abs.ActionDispatch attribute -> pretty "." <> pretty attribute

instance {-# OVERLAPPING #-} Pretty [Abs.ObjectAction] where
  pretty = hsep . fmap pretty
