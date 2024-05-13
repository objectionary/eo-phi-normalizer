-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.EO.Phi.Syntax.Par
  ( happyError
  , myLexer
  , pProgram
  , pObject
  , pBinding
  , pListBinding
  , pAttribute
  , pRuleAttribute
  , pPeeledObject
  , pObjectHead
  , pObjectAction
  , pListObjectAction
  ) where

import Prelude

import qualified Language.EO.Phi.Syntax.Abs
import Language.EO.Phi.Syntax.Lex

}

%name pProgram Program
%name pObject Object
%name pBinding Binding
%name pListBinding ListBinding
%name pAttribute Attribute
%name pRuleAttribute RuleAttribute
%name pPeeledObject PeeledObject
%name pObjectHead ObjectHead
%name pObjectAction ObjectAction
%name pListObjectAction ListObjectAction
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('                { PT _ (TS _ 1)                }
  ')'                { PT _ (TS _ 2)                }
  ','                { PT _ (TS _ 3)                }
  '.'                { PT _ (TS _ 4)                }
  '['                { PT _ (TS _ 5)                }
  ']'                { PT _ (TS _ 6)                }
  '{'                { PT _ (TS _ 7)                }
  '}'                { PT _ (TS _ 8)                }
  'Δ'                { PT _ (TS _ 9)                }
  'Φ'                { PT _ (TS _ 10)               }
  'λ'                { PT _ (TS _ 11)               }
  'ξ'                { PT _ (TS _ 12)               }
  'ρ'                { PT _ (TS _ 13)               }
  'φ'                { PT _ (TS _ 14)               }
  '↦'                { PT _ (TS _ 15)               }
  '∅'                { PT _ (TS _ 16)               }
  '⊥'                { PT _ (TS _ 17)               }
  '⟦'                { PT _ (TS _ 18)               }
  '⟧'                { PT _ (TS _ 19)               }
  '⤍'                { PT _ (TS _ 20)               }
  L_Bytes            { PT _ (T_Bytes $$)            }
  L_Function         { PT _ (T_Function $$)         }
  L_LabelId          { PT _ (T_LabelId $$)          }
  L_AlphaIndex       { PT _ (T_AlphaIndex $$)       }
  L_MetaId           { PT _ (T_MetaId $$)           }
  L_MetaFunctionName { PT _ (T_MetaFunctionName $$) }

%%

Bytes :: { Language.EO.Phi.Syntax.Abs.Bytes }
Bytes  : L_Bytes { Language.EO.Phi.Syntax.Abs.Bytes $1 }

Function :: { Language.EO.Phi.Syntax.Abs.Function }
Function  : L_Function { Language.EO.Phi.Syntax.Abs.Function $1 }

LabelId :: { Language.EO.Phi.Syntax.Abs.LabelId }
LabelId  : L_LabelId { Language.EO.Phi.Syntax.Abs.LabelId $1 }

AlphaIndex :: { Language.EO.Phi.Syntax.Abs.AlphaIndex }
AlphaIndex  : L_AlphaIndex { Language.EO.Phi.Syntax.Abs.AlphaIndex $1 }

MetaId :: { Language.EO.Phi.Syntax.Abs.MetaId }
MetaId  : L_MetaId { Language.EO.Phi.Syntax.Abs.MetaId $1 }

MetaFunctionName :: { Language.EO.Phi.Syntax.Abs.MetaFunctionName }
MetaFunctionName  : L_MetaFunctionName { Language.EO.Phi.Syntax.Abs.MetaFunctionName $1 }

Program :: { Language.EO.Phi.Syntax.Abs.Program }
Program
  : '{' '⟦' ListBinding '⟧' '}' { Language.EO.Phi.Syntax.Abs.Program $3 }

Object :: { Language.EO.Phi.Syntax.Abs.Object }
Object
  : '⟦' ListBinding '⟧' { Language.EO.Phi.Syntax.Abs.Formation $2 }
  | Object '(' ListBinding ')' { Language.EO.Phi.Syntax.Abs.Application $1 $3 }
  | Object '.' Attribute { Language.EO.Phi.Syntax.Abs.ObjectDispatch $1 $3 }
  | 'Φ' { Language.EO.Phi.Syntax.Abs.GlobalObject }
  | 'ξ' { Language.EO.Phi.Syntax.Abs.ThisObject }
  | '⊥' { Language.EO.Phi.Syntax.Abs.Termination }
  | Object '[' 'ξ' '↦' Object ']' { Language.EO.Phi.Syntax.Abs.MetaSubstThis $1 $5 }
  | MetaId { Language.EO.Phi.Syntax.Abs.MetaObject $1 }
  | MetaFunctionName '(' Object ')' { Language.EO.Phi.Syntax.Abs.MetaFunction $1 $3 }

Binding :: { Language.EO.Phi.Syntax.Abs.Binding }
Binding
  : Attribute '↦' Object { Language.EO.Phi.Syntax.Abs.AlphaBinding $1 $3 }
  | Attribute '↦' '∅' { Language.EO.Phi.Syntax.Abs.EmptyBinding $1 }
  | 'Δ' '⤍' Bytes { Language.EO.Phi.Syntax.Abs.DeltaBinding $3 }
  | 'Δ' '⤍' '∅' { Language.EO.Phi.Syntax.Abs.DeltaEmptyBinding }
  | 'λ' '⤍' Function { Language.EO.Phi.Syntax.Abs.LambdaBinding $3 }
  | MetaId { Language.EO.Phi.Syntax.Abs.MetaBindings $1 }
  | 'Δ' '⤍' MetaId { Language.EO.Phi.Syntax.Abs.MetaDeltaBinding $3 }

ListBinding :: { [Language.EO.Phi.Syntax.Abs.Binding] }
ListBinding
  : {- empty -} { [] }
  | Binding { (:[]) $1 }
  | Binding ',' ListBinding { (:) $1 $3 }

Attribute :: { Language.EO.Phi.Syntax.Abs.Attribute }
Attribute
  : 'φ' { Language.EO.Phi.Syntax.Abs.Phi }
  | 'ρ' { Language.EO.Phi.Syntax.Abs.Rho }
  | LabelId { Language.EO.Phi.Syntax.Abs.Label $1 }
  | AlphaIndex { Language.EO.Phi.Syntax.Abs.Alpha $1 }
  | MetaId { Language.EO.Phi.Syntax.Abs.MetaAttr $1 }

RuleAttribute :: { Language.EO.Phi.Syntax.Abs.RuleAttribute }
RuleAttribute
  : Attribute { Language.EO.Phi.Syntax.Abs.ObjectAttr $1 }
  | 'Δ' { Language.EO.Phi.Syntax.Abs.DeltaAttr }
  | 'λ' { Language.EO.Phi.Syntax.Abs.LambdaAttr }

PeeledObject :: { Language.EO.Phi.Syntax.Abs.PeeledObject }
PeeledObject
  : ObjectHead ListObjectAction { Language.EO.Phi.Syntax.Abs.PeeledObject $1 $2 }

ObjectHead :: { Language.EO.Phi.Syntax.Abs.ObjectHead }
ObjectHead
  : '⟦' ListBinding '⟧' { Language.EO.Phi.Syntax.Abs.HeadFormation $2 }
  | 'Φ' { Language.EO.Phi.Syntax.Abs.HeadGlobal }
  | 'ξ' { Language.EO.Phi.Syntax.Abs.HeadThis }
  | '⊥' { Language.EO.Phi.Syntax.Abs.HeadTermination }

ObjectAction :: { Language.EO.Phi.Syntax.Abs.ObjectAction }
ObjectAction
  : '(' ListBinding ')' { Language.EO.Phi.Syntax.Abs.ActionApplication $2 }
  | '.' Attribute { Language.EO.Phi.Syntax.Abs.ActionDispatch $2 }

ListObjectAction :: { [Language.EO.Phi.Syntax.Abs.ObjectAction] }
ListObjectAction
  : {- empty -} { [] } | ObjectAction ListObjectAction { (:) $1 $2 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

