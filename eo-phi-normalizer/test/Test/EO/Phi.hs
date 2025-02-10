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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Test.EO.Phi where

import Control.Monad (forM)
import Data.Aeson (FromJSON (..), SumEncoding (UntaggedValue), defaultOptions, genericParseJSON, sumEncoding)
import Data.List (sort)
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Language.EO.Phi qualified as Phi
import Language.EO.Phi.Rules.Yaml()
import Language.EO.Phi.Syntax
import System.Directory (listDirectory)
import System.FilePath ((</>))

data PhiTestGroup = PhiTestGroup
  { title :: String
  , tests :: [PhiTest]
  }
  deriving (Generic, FromJSON, Show)

data PhiTest = PhiTest
  { name :: String
  , input :: Phi.Program
  , normalized :: Phi.Program
  , prettified :: String
  }
  deriving (Generic, FromJSON, Show)

data DataizeTestGroup = DataizeTestGroup
  { title :: String
  , tests :: [DataizeTest]
  }
  deriving (Generic, FromJSON, Show)

data DataizeTest = DataizeTest
  { name :: String
  , input :: Phi.Program
  , output :: DataizationResult
  , dependencies :: [FilePath]
  }
  deriving (Generic, FromJSON, Show)

data DataizationResult
  = Bytes {bytes :: Phi.Bytes}
  | Object {object :: NoDesugar Phi.Object}
  deriving (Generic, Show)

instance FromJSON DataizationResult where
  parseJSON = genericParseJSON defaultOptions{sumEncoding = UntaggedValue}

fileTests :: FilePath -> IO PhiTestGroup
fileTests = Yaml.decodeFileThrow

dataizationTests :: FilePath -> IO DataizeTestGroup
dataizationTests = Yaml.decodeFileThrow

allPhiTests :: FilePath -> IO [PhiTestGroup]
allPhiTests dir = do
  paths <- listDirectory dir
  forM (sort paths) $ \path ->
    fileTests (dir </> path)

progToObj :: Phi.Program -> Phi.Object
progToObj (Phi.Program bindings) = Phi.Formation bindings
