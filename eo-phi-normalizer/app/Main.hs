{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Control.Monad (unless, when)
import Data.Foldable (forM_)

import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.List (nub)
import Data.String.Interpolate (i)
import Data.Text.Lazy.Lens
import GHC.Generics (Generic)
import Language.EO.Phi (Object (Formation), Program (Program), parseProgram, printTree)
import Language.EO.Phi.Metrics.Collect (collectMetrics)
import Language.EO.Phi.Rules.Common (applyRules, applyRulesChain)
import Language.EO.Phi.Rules.Common qualified as Common
import Language.EO.Phi.Rules.Yaml (RuleSet (rules, title), convertRule, parseRuleSetFromFile)
import Options.Applicative
import Options.Applicative.Types qualified as Optparse (Context (..))
import System.IO (IOMode (WriteMode), hPutStr, hPutStrLn, openFile, stdout)

data CLI'TransformPhi = CLI'TransformPhi
  { chain :: Bool
  , rulesPath :: String
  , outputFile :: Maybe String
  , single :: Bool
  , json :: Bool
  , inputFile :: Maybe FilePath
  , program :: Maybe String
  }
  deriving (Show)

data CLI'MetricsPhi = CLI'MetricsPhi
  { json :: Bool
  , inputFile :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , program :: Maybe String
  }
  deriving (Show)

data CLI
  = CLI'TransformPhi' CLI'TransformPhi
  | CLI'MetricsPhi' CLI'MetricsPhi
  deriving (Show)

_FILE :: String
_FILE = "FILE"

_PROGRAM :: String
_PROGRAM = "PROGRAM"

fileMetavar :: Mod OptionFields a
fileMetavar = metavar _FILE

inputFileLongName :: String
inputFileLongName = "input-file"

inputFileOption :: Parser (Maybe FilePath)
inputFileOption = optional $ strOption (long inputFileLongName <> short 'i' <> help [i|#{_FILE} to read input from. You must specify either this option or #{_PROGRAM}.|] <> fileMetavar)

outputFileOption :: Parser (Maybe String)
outputFileOption = optional $ strOption (long "output-file" <> short 'o' <> help [i|Output to #{_FILE}. Output to stdout otherwise.|] <> fileMetavar)

programArg :: Parser (Maybe String)
programArg = optional $ strArgument (metavar _PROGRAM <> help "Program to work with.")

jsonSwitch :: Parser Bool
jsonSwitch = switch (long "json" <> short 'j' <> help "Output JSON.")

cli'TransformPhi :: Parser CLI'TransformPhi
cli'TransformPhi = do
  rulesPath <- strOption (long "rules" <> short 'r' <> help [i|#{_FILE} with user-defined rules.|] <> fileMetavar)
  inputFile <- inputFileOption
  program <- programArg
  chain <- switch (long "chain" <> short 'c' <> help "Output transformation steps.")
  json <- jsonSwitch
  outputFile <- outputFileOption
  single <- switch (long "single" <> short 's' <> help "Output a single expression.")
  pure CLI'TransformPhi{..}

cli'MetricsPhi :: Parser CLI'MetricsPhi
cli'MetricsPhi = do
  json <- jsonSwitch
  inputFile <- inputFileOption
  outputFile <- outputFileOption
  program <- programArg
  pure CLI'MetricsPhi{..}

metricsParserInfo :: ParserInfo CLI
metricsParserInfo = info (CLI'MetricsPhi' <$> cli'MetricsPhi) (progDesc "Collect metrics for a PHI program.")

transformParserInfo :: ParserInfo CLI
transformParserInfo = info (CLI'TransformPhi' <$> cli'TransformPhi) (progDesc "Transform a PHI program.")

transformCommandName :: String
transformCommandName = "transform"

metricsCommandName :: String
metricsCommandName = "metrics"

cli :: Parser CLI
cli =
  hsubparser
    ( command transformCommandName transformParserInfo
        <> command metricsCommandName metricsParserInfo
    )

cliOpts :: ParserInfo CLI
cliOpts =
  info
    (cli <**> helper)
    (fullDesc <> progDesc "Work with PHI expressions.")

data StructuredJSON = StructuredJSON
  { input :: String
  , output :: [[String]]
  }
  deriving (Generic, ToJSON)

encodeToJSONString :: (ToJSON a) => a -> String
encodeToJSONString = (^. unpacked) . encodeToLazyText

pprefs :: ParserPrefs
pprefs = prefs (showHelpOnEmpty <> showHelpOnError)

type Context = (?parserContext :: Optparse.Context)

die :: (Context) => String -> IO a
die message = do
  handleParseResult . Failure $
    parserFailure pprefs cliOpts (ErrorMsg message) [?parserContext]

getProgram :: (Context) => Maybe FilePath -> Maybe String -> IO Program
getProgram inputFile expression = do
  src <-
    case (inputFile, expression) of
      (Just inputFile', Nothing) -> readFile inputFile'
      (Nothing, Just expression') -> pure expression'
      _ -> die [i|You must specify either -#{head inputFileLongName}|--#{inputFileLongName} #{_FILE} or #{_PROGRAM}|]
  case parseProgram src of
    Left err -> die [i|"An error occurred parsing the input program: #{err}|]
    Right program -> pure program

getLoggers :: Maybe FilePath -> IO (String -> IO (), String -> IO ())
getLoggers outputFile = do
  handle <- maybe (pure stdout) (`openFile` WriteMode) outputFile
  pure (hPutStrLn handle, hPutStr handle)

main :: IO ()
main = do
  opts <- customExecParser pprefs cliOpts
  case opts of
    CLI'MetricsPhi' CLI'MetricsPhi{..} -> do
      let ?parserContext = Optparse.Context metricsCommandName metricsParserInfo
      program' <- getProgram inputFile program
      (logStrLn, _) <- getLoggers outputFile
      let metrics = collectMetrics program'
      logStrLn $ encodeToJSONString metrics
    CLI'TransformPhi' CLI'TransformPhi{..} -> do
      let ?parserContext = Optparse.Context transformCommandName transformParserInfo
      program' <- getProgram inputFile program
      (logStrLn, logStr) <- getLoggers outputFile
      ruleSet <- parseRuleSetFromFile rulesPath
      unless (single || json) $ logStrLn ruleSet.title
      let Program bindings = program'
          results
            | chain = applyRulesChain (Common.Context (convertRule <$> ruleSet.rules) [Formation bindings]) (Formation bindings)
            | otherwise = pure <$> applyRules (Common.Context (convertRule <$> ruleSet.rules) [Formation bindings]) (Formation bindings)
          uniqueResults = nub results
          totalResults = length uniqueResults
      when (null uniqueResults || null (head uniqueResults)) $ die [i|Could not normalize the #{_PROGRAM}.|]
      let printAsProgramOrAsObject = \case
            Formation bindings' -> printTree $ Program bindings'
            x -> printTree x
      if single
        then
          logStrLn
            . (if json then encodeToJSONString else id)
            . printAsProgramOrAsObject
            $ head (head uniqueResults)
        else do
          if json
            then
              logStrLn . encodeToJSONString $
                StructuredJSON
                  { input = printTree program'
                  , output = (printAsProgramOrAsObject <$>) <$> results
                  }
            else do
              logStrLn "Input:"
              logStrLn (printTree program')
              logStrLn "===================================================="
              forM_ (zip [1 ..] uniqueResults) $ \(index, steps) -> do
                logStrLn $
                  "Result " <> show index <> " out of " <> show totalResults <> ":"
                let n = length steps
                forM_ (zip [1 ..] steps) $ \(k, step) -> do
                  when chain $
                    logStr ("[ " <> show k <> " / " <> show n <> " ]")
                  logStrLn . printAsProgramOrAsObject $ step
                logStrLn "----------------------------------------------------"
