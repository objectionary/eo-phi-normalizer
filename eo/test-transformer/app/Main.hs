{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Monad
import Data.Either (partitionEithers)
import Data.List (dropWhileEnd, intercalate, partition)
import Data.List.Extra (split)
import Data.Yaml (FromJSON, ToJSON, decodeFileThrow, encodeFile)
import GHC.Generics (Generic)
import System.Directory.Extra (createDirectoryIfMissing, listFiles, removeFile)
import System.FilePath.Posix
import Text.Printf (printf)
import Main.Utf8 (withUtf8)

main :: IO ()
main = withUtf8 do
    let yamlDir = "test"
        yamlTestDir = yamlDir </> "yaml"
    createDirectoryIfMissing True yamlTestDir
    testFiles <- listFiles "eo-runtime/src/test/eo/org/eolang-original"
    forM_ testFiles $ \f -> do
        eoCode <- readFile f
        let t = lines eoCode
            (license_, t1) = span (\case x : _ -> x == '#'; [] -> False) t
            (meta_, t2) = span (\case x : _ -> x == '+'; [] -> False) (tail t1)
            (bad, good) = partitionEithers (collectPrograms (tail t2))
            test = Test{eo = f, meta = intercalate "\n" meta_, programs = good}
        forM_ bad (\x -> printf "Bad entry: %s:%d\n" f (x + length license_ + 1 + length meta_ + 1))
        let target = yamlTestDir </> dropExtension (takeFileName f) <.> "yaml"
            targetTmp = target <.> ".tmp"
        writeFile target (intercalate "\n" license_ <> "\n\n")
        encodeFile targetTmp test
        readFile targetTmp >>= appendFile target
        removeFile targetTmp

        -- Select subset of tests
        let eoSubsetDir = "eo-runtime/src/test/eo/org/eolang-subset"
        createDirectoryIfMissing True eoSubsetDir
        testsConfig <- decodeFileThrow @_ @Config (yamlDir </> "config.yaml")
        -- print yamlTestsConfig
        forM_ testsConfig $ \testConfig -> do
            testYaml <- decodeFileThrow @_ @Test (yamlTestDir </> (dropExtension testConfig.set) <.> "yaml")
            let programs = filter (\x -> x.name `elem` testConfig.programs) testYaml.programs
                testFile' = eoSubsetDir </> testConfig.set
            writeFile testFile' (testYaml.meta <> "\n\n")
            forM_ programs (\x -> appendFile testFile' (x.text <> "\n"))

-- writeFile eolangTestDir

-- encodeFile (eolangTestDir </> testConfig.set <.> "yaml") testYaml{programs}

data TestConfig = TestConfig
    { set :: String
    , programs :: [String]
    }
    deriving (Show, Generic, FromJSON)

type Config = [TestConfig]

data BadProgram = BadProgram
    { file :: FilePath
    , line :: Int
    }
    deriving (Show)

data Program = Program
    { name :: String
    , text :: String
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data Test = Test
    { eo :: String
    , meta :: String
    , programs :: [Program]
    }
    deriving (Show, Generic, ToJSON, FromJSON)

collectPrograms :: [String] -> [Either Int Program]
collectPrograms ts =
    let
        (empties, texts) = partition (== []) (split (== "# Test.") ts)
        go (res, ptr) (x : xs)
            | "" `elem` dropWhile (== "") (dropWhileEnd (== "") x) = go (res <> [Left ptr], ptr + length x + 1) xs
            | otherwise =
                go
                    ( res <> [Right Program{name = drop 5 (head (dropWhile (\y -> y /= [] && head y /= '[') x)), text = intercalate "\n" x}]
                    , ptr + length x + 1
                    )
                    xs
        go (res, _) [] = res
     in
        go ([], length empties) texts
