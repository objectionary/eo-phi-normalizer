{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Control.Monad (unless, when)
import Data.Foldable (forM_)

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Function ((&))
import Data.List (nub, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy.Lens
import Language.EO.Phi (Object (Formation), Program (Program), defaultMain, parseProgram, printTree)
import Language.EO.Phi.Metrics.Collect (collectMetrics)
import Language.EO.Phi.Rules.Common (Context (..), applyRules, applyRulesChain)
import Language.EO.Phi.Rules.Yaml (RuleSet (rules, title), convertRule, parseRuleSetFromFile)
import Options.Generic
import System.IO (IOMode (WriteMode), hClose, hPutStr, hPutStrLn, openFile, stdout)

data CLI'TransformPhi'Params = CLI'TransformPhi'Params
  { chain :: Bool
  , rulesYaml :: Maybe String
  , outPath :: Maybe String
  , single :: Bool
  , json :: Bool
  }
  deriving (Generic, Show, ParseRecord, Read, ParseField)

instance ParseFields CLI'TransformPhi'Params where
  parseFields _ _ _ _ =
    CLI'TransformPhi'Params
      <$> parseFields (Just "Print out steps of reduction") (Just "chain") (Just 'c') Nothing
      <*> parseFields (Just "Path to the Yaml file with custom rules") (Just "rules-yaml") Nothing Nothing
      <*> parseFields (Just "Output file path (defaults to stdout)") (Just "output") (Just 'o') Nothing
      <*> parseFields (Just "Print a single normalized expression") (Just "single") (Just 's') Nothing
      <*> parseFields (Just "Print JSON") (Just "json") (Just 'j') Nothing

data CLI
  = CLI'TransformPhi CLI'TransformPhi'Params (Maybe FilePath)
  | CLI'MetricsPhi (Maybe FilePath)
  deriving (Generic, Show)

modifiers :: Modifiers
modifiers =
  lispCaseModifiers
    { constructorNameModifier = \x ->
        stripPrefix "CLI'" x
          & fromMaybe ""
          & lispCaseModifiers.constructorNameModifier
    , shortNameModifier = firstLetter
    }

instance ParseRecord CLI where
  parseRecord = parseRecordWithModifiers modifiers

data StructuredJSON = StructuredJSON
  { input :: String
  , results :: [[String]]
  }
  deriving (Generic, ToJSON)

encodeToJSONString :: (ToJSON a) => a -> String
encodeToJSONString = (^. unpacked) . encodeToLazyText

main :: IO ()
main = do
  opts <- getRecord "Normalizer"
  case opts of
    CLI'MetricsPhi inPath -> do
      src <- maybe getContents readFile inPath
      let progOrError = parseProgram src
      case progOrError of
        Left err -> error ("An error occurred parsing the input program: " <> err)
        Right a -> do
          let handle = stdout
          let logStrLn = hPutStrLn handle
              metrics = collectMetrics a
          logStrLn $ encodeToJSONString metrics
    CLI'TransformPhi params inPath -> do
      let (CLI'TransformPhi'Params{..}) = params
      case rulesYaml of
        Just path -> do
          handle <- maybe (pure stdout) (`openFile` WriteMode) outPath
          let logStr = hPutStr handle
              logStrLn = hPutStrLn handle
          ruleSet <- parseRuleSetFromFile path
          unless (single || json) $ logStrLn ruleSet.title
          src <- maybe getContents readFile inPath
          let progOrError = parseProgram src
          case progOrError of
            Left err -> error ("An error occurred parsing the input program: " <> err)
            Right input@(Program bindings) -> do
              let
                results
                  | chain = applyRulesChain (Context (convertRule <$> ruleSet.rules) [Formation bindings]) (Formation bindings)
                  | otherwise = pure <$> applyRules (Context (convertRule <$> ruleSet.rules) [Formation bindings]) (Formation bindings)
                uniqueResults = nub results
                totalResults = length uniqueResults
              when (null uniqueResults || null (head uniqueResults)) $ error "Could not normalize the program"
              let printFormationAsProgramOrObject = \case
                    Formation bindings' -> printTree $ Program bindings'
                    x -> printTree x
              if single
                then
                  logStrLn
                    . (if json then encodeToJSONString else id)
                    . printFormationAsProgramOrObject
                    $ head (head uniqueResults)
                else do
                  if json
                    then
                      logStrLn . encodeToJSONString $
                        StructuredJSON
                          { input = printTree input
                          , results = (printFormationAsProgramOrObject <$>) <$> results
                          }
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
                          logStrLn . printFormationAsProgramOrObject $ step
                        logStrLn "----------------------------------------------------"
          hClose handle
        -- TODO #48:15m still need to consider `chain` (should rewrite/change defaultMain to mainWithOptions)
        Nothing -> defaultMain
