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

module Language.EO.Phi.Dataize.Context where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet as HashSet (difference, fromList, intersection, member)
import Data.List.NonEmpty qualified as NonEmpty
import Language.EO.Phi.Dataize.Atoms as Atoms
import Language.EO.Phi.Rules.Common
import Language.EO.Phi.Syntax.Abs

knownAtomsMap :: Atoms
knownAtomsMap = HashMap.fromList knownAtomsList

defaultContext :: [NamedRule] -> Object -> Context
defaultContext rules obj =
  Context
    { builtinRules = False
    , allRules = rules
    , enabledAtoms = mkEnabledAtoms [] []
    , knownAtoms = knownAtomsMap
    , outerFormations = NonEmpty.singleton obj
    , currentAttr = Phi
    , insideFormation = False
    , insideAbstractFormation = False
    , dataizePackage = True
    , minimizeTerms = False
    , insideSubObject = False
    }

mkEnabledAtoms :: [String] -> [String] -> Atoms
mkEnabledAtoms enabled disabled = enabledAtoms'
 where
  knownAtomsSet = HashSet.fromList (HashMap.keys knownAtomsMap)
  disabledSet = HashSet.fromList disabled
  enabledSet =
    case enabled of
      [] -> knownAtomsSet
      _ -> HashSet.fromList enabled
  enabled' = HashSet.intersection (HashSet.difference enabledSet disabledSet) knownAtomsSet
  enabledAtoms' = HashMap.filterWithKey (\k _ -> k `HashSet.member` enabled') knownAtomsMap
