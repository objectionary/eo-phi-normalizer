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
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.EO.Phi.Syntax (
  module Language.EO.Phi.Syntax.Abs,
  printTree,
  shrinkDots,
) where

import Data.Char (isSpace)
import Language.EO.Phi.Rules.Common ()
import Language.EO.Phi.Syntax.Abs
import Language.EO.Phi.Syntax.Print qualified as Phi

-- * Overriding generated pretty-printer

-- | Like 'Phi.printTree', but without spaces around dots and no indentation for curly braces.
printTree :: (Phi.Print a) => a -> String
printTree = shrinkDots . render . Phi.prt 0

-- | Remove spaces around dots.
--
-- >>> shrinkDots "a ↦ ξ . a" == "a ↦ ξ.a"
-- True
shrinkDots :: String -> String
shrinkDots [] = []
shrinkDots (' ' : '.' : ' ' : cs) = '.' : shrinkDots cs
shrinkDots (c : cs) = c : shrinkDots cs

-- | Copy of 'Phi.render', except no indentation is made for curly braces.
render :: Phi.Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
 where
  rend ::
    Int ->
    -- \^ Indentation level.
    Bool ->
    -- \^ Pending indentation to be output before next character?
    [String] ->
    ShowS
  rend i p = \case
    "[" : "]" : ts -> showString "[]" . rend i False ts
    "(" : ")" : (t : ts) -> handleTrailingComma "()" t ts
    "⟦" : "⟧" : (t : ts) -> handleTrailingComma "⟦⟧" t ts
    "[" : ts -> char '[' . rend i False ts
    "(" : ts -> char '(' . new (i + 1) ts
    "{" : "⟦" : ts -> showChar '{' . onNewLine (i + 1) p . showChar '⟦' . new (i + 2) ts
    "⟦" : ts -> showChar '⟦' . new (i + 1) ts
    ")" : "," : ts -> onNewLine (i - 1) p . showString ")," . new (i - 1) ts
    "⟧" : "," : ts -> onNewLine (i - 1) p . showString "⟧," . new (i - 1) ts
    ["⟧", "}"] -> onNewLine (i - 1) p . showChar '⟧' . new (i - 2) ["}"]
    "⟧" : ts -> onNewLine (i - 1) p . showChar '⟧' . new (i - 1) ts
    ")" : ts -> onNewLine (i - 1) p . showChar ')' . new (i - 1) ts
    [";"] -> char ';'
    ";" : ts -> char ';' . new i ts
    "." : ts -> rend i p (" ." : ts)
    t : (s : ss) | closingOrPunctuation s -> handleTrailingComma t s ss
    t : ts -> pending . space t . rend i False ts
    [] -> id
   where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    handleTrailingComma str t ts =
      (pending . showString str)
        . ( case t of
              "," -> showChar ',' . new i ts
              _ -> rend i False (t : ts)
          )

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = Phi.replicateS (2 * i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True, _, True) -> [] -- remove trailing space
      (False, _, True) -> t -- remove trailing space
      (False, True, False) -> t ++ ' ' : s -- add space if none
      _ -> t ++ s
   where
    (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _ = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i
