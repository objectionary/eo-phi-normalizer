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
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.ParserSpec where

import Control.Monad (forM_)
import Test.Hspec

import Data.Aeson (FromJSON)
import Data.Either (isLeft, isRight)
import Data.Yaml (decodeFileThrow)
import GHC.Generics (Generic)
import Language.EO.Phi (parseProgram)

data ParserTests = ParserTests
  { title :: String
  , tests :: TestTypes
  }
  deriving (Generic, FromJSON)

data TestTypes = TestTypes
  { positive :: [ParserTest]
  , negative :: [ParserTest]
  }
  deriving (Generic, FromJSON)

data ParserTest = ParserTest
  { title :: String
  , source :: String
  , input :: String
  }
  deriving (Generic, FromJSON)

spec :: Spec
spec = do
  ParserTests{..} <- runIO (decodeFileThrow "test/eo/phi/parser/expressions.yaml")
  describe title do
    forM_
      [ ("Positive", tests.positive, isRight)
      , ("Negative", tests.negative, isLeft)
      ]
      $ \(title', set, check) ->
        describe title' do
          forM_ set $ \test -> do
            let p = parseProgram test.input
            it test.title do
              shouldSatisfy p check
