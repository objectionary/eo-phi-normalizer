{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePrettyToTextBuilder')
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy.Lens
import GHC.Generics (Generic)
import Language.EO.Phi (Attribute (Sigma), Object (Formation), Program (Program), parseProgram, printTree)
import Language.EO.Phi.Metrics (collectBindingsMetrics, programBindingsByPath)
import Language.EO.Phi.Rules.Common (ApplicationLimits (ApplicationLimits), Context (..), applyRulesChainWith, applyRulesWith, objectSize)
import Language.EO.Phi.Rules.Yaml (RuleSet (rules, title), convertRule, parseRuleSetFromFile)
import Options.Applicative
import Options.Applicative.Types qualified as Optparse (Context (..))
import System.IO (IOMode (WriteMode), getContents', hFlush, hPutStr, hPutStrLn, openFile, stdout)

data CLI'TransformPhi = CLI'TransformPhi
  { chain :: Bool
  , rulesPath :: String
  , outputFile :: Maybe String
  , single :: Bool
  , json :: Bool
  , inputFile :: Maybe FilePath
  , maxDepth :: Int
  , maxGrowthFactor :: Int
  }
  deriving (Show)

data CLI'MetricsPhi = CLI'MetricsPhi
  { inputFile :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , programPath :: Maybe String
  }
  deriving (Show)

data CLI
  = CLI'TransformPhi' CLI'TransformPhi
  | CLI'MetricsPhi' CLI'MetricsPhi
  deriving (Show)

fileMetavarName :: String
fileMetavarName = "FILE"

fileMetavar :: Mod OptionFields a
fileMetavar = metavar fileMetavarName

outputFileOption :: Parser (Maybe String)
outputFileOption = optional $ strOption (long "output-file" <> short 'o' <> help [i|Output to #{fileMetavarName}. When this option is not specified, output to stdout.|] <> fileMetavar)

inputFileArg :: Parser (Maybe String)
inputFileArg = optional $ strArgument (metavar fileMetavarName <> help [i|#{fileMetavarName} to read input from. When no #{fileMetavarName} is specified, read from stdin.|])

jsonSwitch :: Parser Bool
jsonSwitch = switch (long "json" <> short 'j' <> help "Output JSON.")

cliTransformPhiParser :: Parser CLI'TransformPhi
cliTransformPhiParser = do
  rulesPath <- strOption (long "rules" <> short 'r' <> help [i|#{fileMetavarName} with user-defined rules. Must be specified.|] <> fileMetavar)
  chain <- switch (long "chain" <> short 'c' <> help "Output transformation steps.")
  json <- jsonSwitch
  outputFile <- outputFileOption
  single <- switch (long "single" <> short 's' <> help "Output a single expression.")
  maxDepth <- option auto (long "max-depth" <> metavar "INT" <> value 10 <> help "Maximum depth of rules application. Defaults to 10.")
  maxGrowthFactor <- option auto (long "max-growth-factor" <> metavar "INT" <> value 10 <> help "The factor by which to allow the input term to grow before stopping. Defaults to 10.")
  inputFile <- inputFileArg
  pure CLI'TransformPhi{..}

programPathOption :: Parser (Maybe String)
programPathOption =
  optional $
    strOption
      ( long "program-path"
          <> short 'p'
          <> metavar "PATH"
          <> help [i|Report metrics for attributes accessible in a program via PATH. Defaults to an empty path. Example: "org.eolang"|]
      )

cliMetricsPhiParser :: Parser CLI'MetricsPhi
cliMetricsPhiParser = do
  inputFile <- inputFileArg
  outputFile <- outputFileOption
  programPath <- programPathOption
  pure CLI'MetricsPhi{..}

metricsParserInfo :: ParserInfo CLI
metricsParserInfo = info (CLI'MetricsPhi' <$> cliMetricsPhiParser) (progDesc "Collect metrics for a PHI program.")

transformParserInfo :: ParserInfo CLI
transformParserInfo = info (CLI'TransformPhi' <$> cliTransformPhiParser) (progDesc "Transform a PHI program.")

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
encodeToJSONString = (^. unpacked) . toLazyText . encodePrettyToTextBuilder' defConfig{confIndent = Spaces 2}

pprefs :: ParserPrefs
pprefs = prefs (showHelpOnEmpty <> showHelpOnError)

die :: Optparse.Context -> String -> IO a
die parserContext message = do
  handleParseResult . Failure $
    parserFailure pprefs cliOpts (ErrorMsg message) [parserContext]

getProgram :: Optparse.Context -> Maybe FilePath -> IO Program
getProgram parserContext inputFile = do
  src <- maybe getContents' readFile inputFile
  case parseProgram src of
    Left err -> die parserContext [i|An error occurred when parsing the input program: #{err}|]
    Right program -> pure program

getLoggers :: Maybe FilePath -> IO (String -> IO (), String -> IO ())
getLoggers outputFile = do
  handle <- maybe (pure stdout) (`openFile` WriteMode) outputFile
  pure
    ( \x -> hPutStrLn handle x >> hFlush handle
    , \x -> hPutStr handle x >> hFlush handle
    )

-- >>> splitStringOn "." "abra.cada.bra"
-- ["abra","cada","bra"]
--
-- >>> splitStringOn "." ""
-- []
splitStringOn :: String -> String -> [String]
splitStringOn sep s = filter (not . null) $ T.unpack <$> T.splitOn (T.pack sep) (T.pack s)

main :: IO ()
main = do
  opts <- customExecParser pprefs cliOpts
  case opts of
    CLI'MetricsPhi' CLI'MetricsPhi{..} -> do
      let parserContext = Optparse.Context metricsCommandName metricsParserInfo
      program <- getProgram parserContext inputFile
      (logStrLn, _) <- getLoggers outputFile
      let path = splitStringOn "." (fromMaybe "" programPath)
          metrics = collectBindingsMetrics (programBindingsByPath path program)
      logStrLn $ encodeToJSONString metrics
    CLI'TransformPhi' CLI'TransformPhi{..} -> do
      let parserContext = Optparse.Context transformCommandName transformParserInfo
      program' <- getProgram parserContext inputFile
      (logStrLn, logStr) <- getLoggers outputFile
      ruleSet <- parseRuleSetFromFile rulesPath
      unless (single || json) $ logStrLn ruleSet.title
      let Program bindings = program'
          uniqueResults
            | chain = applyRulesChainWith limits (Context (convertRule <$> ruleSet.rules) [Formation bindings] Sigma) (Formation bindings)
            | otherwise = pure <$> applyRulesWith limits (Context (convertRule <$> ruleSet.rules) [Formation bindings] Sigma) (Formation bindings)
           where
            limits = ApplicationLimits maxDepth (maxGrowthFactor * objectSize (Formation bindings))
          totalResults = length uniqueResults
      when (null uniqueResults || null (head uniqueResults)) $ die parserContext [i|Could not normalize the program.|]
      let printAsProgramOrAsObject = \case
            Formation bindings' -> printTree $ Program bindings'
            x -> printTree x
      if
        | single && json ->
            logStrLn
              . encodeToJSONString
              . printAsProgramOrAsObject
              $ head (head uniqueResults)
        | single ->
            logStrLn
              . printAsProgramOrAsObject
              $ head (head uniqueResults)
        | json ->
            logStrLn . encodeToJSONString $
              StructuredJSON
                { input = printTree program'
                , output = (printAsProgramOrAsObject <$>) <$> uniqueResults
                }
        | otherwise -> do
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
