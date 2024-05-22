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
    test@Test{source, meta} <- parseTest testSet.source
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
    createDirectoryIfMissing True (takeDirectory testSet.destination)
    writeFile testSet.destination meta
    forM_ programs (\x -> appendFile testSet.destination x.text)

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
