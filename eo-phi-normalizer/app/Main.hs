{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Monad (unless, when)
import Data.Foldable (forM_)

import Data.List (nub)
import Language.EO.Phi (Object (Formation), Program (Program), defaultMain, parseProgram, printTree)
import Language.EO.Phi.Rules.Common (Context (..), applyRules, applyRulesChain)
import Language.EO.Phi.Rules.Yaml (RuleSet (rules, title), convertRule, parseRuleSetFromFile)
import Options.Generic
import System.IO (IOMode (WriteMode), hClose, hPutStr, hPutStrLn, openFile, stdout)

data CLINamedParams = CLINamedParams
  { chain :: Bool
  , rulesYaml :: Maybe String
  , outPath :: Maybe String
  , single :: Bool
  }
  deriving (Generic, Show, ParseRecord, Read, ParseField)

instance ParseFields CLINamedParams where
  parseFields _ _ _ _ =
    CLINamedParams
      <$> parseFields (Just "Print out steps of reduction") (Just "chain") (Just 'c') Nothing
      <*> parseFields (Just "Path to the Yaml file with custom rules") (Just "rules-yaml") Nothing Nothing
      <*> parseFields (Just "Output file path (defaults to stdout)") (Just "output") (Just 'o') Nothing
      <*> parseFields (Just "Print a single normlized expression") (Just "single") (Just 's') Nothing

data CLIOptions = CLIOptions CLINamedParams (Maybe FilePath)
  deriving (Generic, Show, ParseRecord)

main :: IO ()
main = do
  opts <- getRecord "Normalizer"
  let (CLIOptions params inPath) = opts
  let (CLINamedParams{..}) = params
  case rulesYaml of
    Just path -> do
      handle <- maybe (pure stdout) (`openFile` WriteMode) outPath
      let logStr = hPutStr handle
      let logStrLn = hPutStrLn handle
      ruleSet <- parseRuleSetFromFile path
      unless single $ logStrLn ruleSet.title
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
          when (totalResults == 0) $ error "Could not normalize the program"
          if single
            then logStrLn (printTree (head uniqueResults))
            else do
              logStrLn "Input:"
              logStrLn (printTree input)
              logStrLn "===================================================="
              forM_ (zip [1 ..] uniqueResults) $ \(i, steps) -> do
                logStrLn $
                  "Result " <> show i <> " out of " <> show totalResults <> ":"
                let n = length steps
                forM_ (zip [1 ..] steps) $ \(k, step) -> do
                  when chain $
                    logStr ("[ " <> show k <> " / " <> show n <> " ]")
                  logStrLn (printTree step)
                logStrLn "----------------------------------------------------"
      hClose handle
    -- TODO #48:15m still need to consider `chain` (should rewrite/change defaultMain to mainWithOptions)
    Nothing -> defaultMain
