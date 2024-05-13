module Language.EO.Phi.Rules.PhiPaper where

import Control.Monad (guard)
import Language.EO.Phi
import Language.EO.Phi.Rules.Common

-- * Yegor's Rules

-- | Rule 6.
rule6 :: Rule
rule6 ctx (ObjectDispatch (Formation bindings) a)
  | Just obj <- lookupBinding a bindings = do
      guard (isNF ctx obj)
      return (Application obj [AlphaBinding Rho (Formation bindings')])
 where
  bindings' = filter (not . isDispatched) bindings
  isDispatched (AlphaBinding a' _) = a == a'
  isDispatched _ = False
rule6 _ _ = []
