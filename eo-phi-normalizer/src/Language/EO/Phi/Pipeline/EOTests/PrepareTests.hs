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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.EO.Phi.Pipeline.EOTests.PrepareTests where

import Control.Monad
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Yaml (encodeFile)
import Language.EO.Phi.Pipeline.Config
import Language.EO.Phi.Pipeline.EOTests.Data
import System.Directory
import System.FilePath.Posix

prepareTests :: PipelineConfig -> IO ()
prepareTests config = do
  forM_ (filter (fromMaybe True . (.enable)) config.testSets) $ \((.eo) -> testSet) -> do
    test@Test{source, meta} <- parseTest testSet.original
    let exclude = fromMaybe [] testSet.exclude
        include = fromMaybe (test.programs <&> (.name)) testSet.include & filter (`notElem` exclude)
        programs = filter (\x -> x.name `elem` include) test.programs
        testContent = TestContent{..}

    -- write yaml
    let target = testSet.yaml
        targetTmp = target <.> ".tmp"
    createDirectoryIfMissing True (takeDirectory target)
    encodeFile targetTmp testContent
    readFile targetTmp >>= appendFile target
    removeFile targetTmp

    -- write eo
    createDirectoryIfMissing True (takeDirectory testSet.filtered)
    writeFile testSet.filtered meta
    forM_ programs (\x -> appendFile testSet.filtered x.text)

parseProgramsRaw :: ([(Int, [String])], (Int, [[Char]]), Int) -> [[Char]] -> [(Int, String)]
parseProgramsRaw (programs', (programStart, program), curLine) (line'@(x : _) : xs)
  | (program /= [] && head program == "" || null program) && (x == '[' || x == '#') = parseProgramsRaw ((programStart, program) : programs', (curLine, [line']), curLine + 1) xs
  | otherwise = parseProgramsRaw (programs', (programStart, line' : program), curLine + 1) xs
parseProgramsRaw (programs', (programStart, program), curLine) ("" : xs) = parseProgramsRaw (programs', (programStart, "" : program), curLine + 1) xs
parseProgramsRaw (programs', program, _) [] = (unlines <$>) <$> drop 1 (reverse ((reverse <$>) <$> (program : programs')))

parseTest' :: FilePath -> [String] -> Test
parseTest' source eoCode =
  let
    (license, k') = span (\case '#' : _ -> True; "" -> True; _ -> False) eoCode
    (meta, k'') = span (\case '+' : _ -> True; "" -> True; _ -> False) k'
    programsStart = length license + length meta + 1
    programsRaw = parseProgramsRaw ([], (programsStart, []), programsStart) k''
    programs = programsRaw <&> (\(line, text) -> Program{source = Pos{file = source, ..}, name = text & dropWhile (/= '[') & drop 5 & takeWhile (/= '\n'), ..})
   in
    Test{license = unlines license, meta = unlines meta, ..}

parseTest :: FilePath -> IO Test
parseTest path = readFile path <&> (parseTest' path . lines)
