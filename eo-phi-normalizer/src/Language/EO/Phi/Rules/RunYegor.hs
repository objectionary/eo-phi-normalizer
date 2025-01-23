{- FOURMOLU_DISABLE -}
-- The MIT License (MIT)

-- Copyright (c) 2016-2025 Objectionary.com

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
module Language.EO.Phi.Rules.RunYegor where

import Language.EO.Phi.Dataize.Context
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Rules.Yaml qualified as Yaml
import Language.EO.Phi.Syntax (printTree)
import Language.EO.Phi.Syntax.Abs
import System.IO.Unsafe (unsafePerformIO)

runWithYegorRules :: (Context -> Object -> Object) -> Object -> IO ()
runWithYegorRules f obj = putStrLn (printTree (f (defaultContext yegorRules obj) obj))

yegorRuleSet :: Yaml.RuleSet
{-# NOINLINE yegorRuleSet #-}
yegorRuleSet =
  unsafePerformIO $
    Yaml.parseRuleSetFromFile "eo-phi-normalizer/test/eo/phi/rules/yegor.yaml"

yegorRules :: [NamedRule]
yegorRules = map Yaml.convertRuleNamed (Yaml.rules yegorRuleSet)
