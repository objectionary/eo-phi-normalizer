{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Monad (when)
import Data.Foldable (forM_)

import Data.List (nub)
import Language.EO.Phi (Object (Formation), Program (Program), defaultMain, parseProgram, printTree)
import Language.EO.Phi.Rules.Common (Context (..), applyRules, applyRulesChain)
import Language.EO.Phi.Rules.Yaml
import Options.Generic

data CLINamedParams = CLINamedParams
  { chain :: Bool
  , rulesYaml :: Maybe String
  , outPath :: Maybe String
  }
  deriving (Generic, Show, ParseRecord, Read, ParseField)

instance ParseFields CLINamedParams where
  parseFields _ _ _ _ =
    CLINamedParams
      <$> parseFields (Just "Print out steps of reduction") (Just "chain") (Just 'c') Nothing
      <*> parseFields (Just "Path to the Yaml file with custom rules") (Just "rules-yaml") Nothing Nothing
      <*> parseFields (Just "Output file path (defaults to stdout)") (Just "output") (Just 'o') Nothing

data CLIOptions = CLIOptions CLINamedParams (Maybe FilePath)
  deriving (Generic, Show, ParseRecord)

main :: IO ()
main = do
  opts <- getRecord "Normalizer"
  let (CLIOptions params inPath) = opts
  let (CLINamedParams{..}) = params
  Control.Monad.when chain (putStrLn "Sorry, --chain is not implemented yet 😅")
  case rulesYaml of
    Just path -> do
      ruleSet <- parseRuleSetFromFile path
      putStrLn ruleSet.title
      src <- maybe getContents readFile inPath
      let progOrError = parseProgram src
      case progOrError of
        Left err -> error ("An error occurred parsing the input program: " <> err)
        Right input@(Program bindings) -> do
          let results
                | chain = applyRulesChain (Context (convertRule <$> ruleSet.rules)) (Formation bindings)
                | otherwise = pure <$> applyRules (Context (convertRule <$> ruleSet.rules)) (Formation bindings)
              uniqueResults = nub results
              totalResults = length uniqueResults
          -- TODO: use outPath to output to file if provided
          putStrLn "Input:"
          putStrLn (printTree input)
          putStrLn "===================================================="
          forM_ (zip [1 ..] uniqueResults) $ \(i, steps) -> do
            putStrLn $
              "Result " <> show i <> " out of " <> show totalResults <> ":"
            let n = length steps
            forM_ (zip [1 ..] steps) $ \(k, step) -> do
              Control.Monad.when chain $ do
                putStr ("[ " <> show k <> " / " <> show n <> " ]")
              putStrLn (printTree step)
            putStrLn "----------------------------------------------------"
    -- TODO: still need to consider `chain`
    Nothing -> defaultMain
