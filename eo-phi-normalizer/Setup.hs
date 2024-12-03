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

{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- Source: https://github.com/haskell/cabal/issues/6726#issuecomment-918663262

-- | Custom Setup that runs bnfc to generate the language sub-libraries
-- for the parsers included in Ogma.
module Main (main) where

import Data.List (intercalate)
import Distribution.Simple (defaultMainWithHooks, hookedPrograms, postConf, preBuild, simpleUserHooks)
import Distribution.Simple.Program (Program (..), findProgramVersion, simpleProgram)
import System.Exit (ExitCode (..))
import System.Process (callCommand)
import Control.Exception (evaluate)
import Main.Utf8 (withUtf8)
import System.IO.CodePage (withCP65001)
import Data.ByteString as BS (readFile, writeFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import PyF (fmt)

readFile' :: FilePath -> IO Text
readFile' = (decodeUtf8 <$>) . BS.readFile

writeFile' :: FilePath -> Text -> IO ()
writeFile' path = BS.writeFile path . encodeUtf8

-- | Run BNFC, happy, and alex on the grammar before the actual build step.
--
-- All options for bnfc are hard-coded here.
main :: IO ()
main = withCP65001 . withUtf8 $
  defaultMainWithHooks $
    simpleUserHooks
      { hookedPrograms = [bnfcProgram]
      , postConf = \args flags packageDesc localBuildInfo -> do
          let
            isWindows =
#ifdef mingw32_HOST_OS
                      True
#else
                      False
#endif
            -- See the details on the command form in https://github.com/objectionary/eo-phi-normalizer/issues/347#issuecomment-2117097070
            addLicense :: FilePath -> IO ()
            addLicense file = do
              let targetFile = "src/Language/EO/Phi/Syntax/" <> file
              license <- readFile' "LICENSE"
              let licenseFormatted = [fmt|{{-\n{license}-}}\n\n|] :: Text
              code <- readFile' targetFile
              writeFile' targetFile (licenseFormatted <> code)

            command = intercalate "; " $
                [ "set -ex" ] <>
                [ "chcp.com" | isWindows ] <>
                [ "chcp.com 65001" | isWindows ] <>
                [ "bnfc --haskell -d -p Language.EO.Phi --generic -o src/ grammar/EO/Phi/Syntax.cf"] <>
                [ "cd src/Language/EO/Phi/Syntax" ] <>
                [ "alex Lex.x" ] <>
                [ "happy Par.y" ] <>
                [ "true" ]

            fullCommand = [fmt|bash -c ' {command} '|]

          putStrLn fullCommand

          _ <- callCommand fullCommand
          _ <- addLicense "Abs.hs"
          _ <- addLicense "Print.hs"

          postConf simpleUserHooks args flags packageDesc localBuildInfo
      }

-- | NOTE: This should be in Cabal.Distribution.Simple.Program.Builtin.
bnfcProgram :: Program
bnfcProgram =
  (simpleProgram "bnfc")
    { programFindVersion = findProgramVersion "--version" id
    }
