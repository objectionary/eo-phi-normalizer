{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Control.Monad (unless, when)
import Data.Foldable (forM_)

import Control.Exception (Exception, SomeException, throw)
import Control.Exception qualified as Exception (handle)
import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePrettyToTextBuilder')
import Data.List (groupBy, intercalate)
import Data.String.Interpolate (i, iii)
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy.Lens
import GHC.Generics (Generic)
import Language.EO.Phi (Bytes (Bytes), Object (Formation), Program (Program), parseProgram, printTree)
import Language.EO.Phi.Dataize (dataizeRecursively, dataizeStep)
import Language.EO.Phi.Metrics (ProgramMetrics, getProgramMetrics)
import Language.EO.Phi.Rules.Common (ApplicationLimits (ApplicationLimits), applyRulesChainWith, applyRulesWith, defaultContext, objectSize)
import Language.EO.Phi.Rules.Yaml (RuleSet (rules, title), convertRule, parseRuleSetFromFile)
import Options.Applicative hiding (metavar)
import Options.Applicative qualified as Optparse (metavar)
import Options.Applicative.Types qualified as Optparse (Context (..))
import System.Directory (doesFileExist)
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

data CLI'DataizePhi = CLI'DataizePhi
  { rulesPath :: String
  , inputFile :: Maybe FilePath
  , outputFile :: Maybe String
  , recursive :: Bool
  }
  deriving (Show)

data CLI'MetricsPhi = CLI'MetricsPhi
  { inputFile :: Maybe FilePath
  , outputFile :: Maybe FilePath
  , bindingsPath :: Maybe String
  }
  deriving (Show)

data CLI
  = CLI'TransformPhi' CLI'TransformPhi
  | CLI'DataizePhi' CLI'DataizePhi
  | CLI'MetricsPhi' CLI'MetricsPhi
  deriving (Show)

fileMetavarName :: String
fileMetavarName = "FILE"

data MetavarName = MetavarName
  { file :: String
  , int :: String
  , path :: String
  }

metavarName :: MetavarName
metavarName =
  MetavarName
    { file = "FILE"
    , int = "INT"
    , path = "PATH"
    }

data Metavar a b = Metavar
  { file :: Mod a b
  , int :: Mod a b
  , path :: Mod a b
  }

metavar :: (HasMetavar a) => Metavar a b
metavar =
  Metavar
    { file = Optparse.metavar metavarName.file
    , int = Optparse.metavar metavarName.int
    , path = Optparse.metavar metavarName.path
    }

newtype OptionName = OptionName
  { bindingsPath :: String
  }

optionName :: OptionName
optionName =
  OptionName
    { bindingsPath = "bindings-path"
    }

outputFileOption :: Parser (Maybe String)
outputFileOption = optional $ strOption (long "output-file" <> short 'o' <> metavar.file <> help [i|Output to #{fileMetavarName}. When this option is not specified, output to stdout.|])

inputFileArg :: Parser (Maybe String)
inputFileArg = optional $ strArgument (metavar.file <> help [i|#{fileMetavarName} to read input from. When no #{fileMetavarName} is specified, read from stdin.|])

jsonSwitch :: Parser Bool
jsonSwitch = switch (long "json" <> short 'j' <> help "Output JSON.")

bindingsPathOption :: Parser (Maybe String)
bindingsPathOption =
  optional $
    strOption
      ( long optionName.bindingsPath
          <> short 'b'
          <> metavar.path
          <> help
            let path' = metavarName.path
             in [iii|
                  Report metrics for bindings of a formation accessible in a program by the #{path'}.
                  When this option is not specified, metrics for bindings are not reported.
                  Example of a #{path'}: 'org.eolang'.
                |]
      )

data CommandParser = CommandParser
  { metrics :: Parser CLI'MetricsPhi
  , transform :: Parser CLI'TransformPhi
  , dataize :: Parser CLI'DataizePhi
  }

commandParser :: CommandParser
commandParser =
  CommandParser{..}
 where
  metrics = do
    inputFile <- inputFileArg
    outputFile <- outputFileOption
    bindingsPath <- bindingsPathOption
    pure CLI'MetricsPhi{..}

  transform = do
    rulesPath <-
      let file' = metavarName.file
       in strOption (long "rules" <> short 'r' <> metavar.file <> help [i|#{file'} with user-defined rules. Must be specified.|])
    chain <- switch (long "chain" <> short 'c' <> help "Output transformation steps.")
    json <- jsonSwitch
    outputFile <- outputFileOption
    single <- switch (long "single" <> short 's' <> help "Output a single expression.")
    maxDepth <-
      let maxValue = 10
       in option auto (long "max-depth" <> metavar.int <> value maxValue <> help [i|Maximum depth of rules application. Defaults to #{maxValue}.|])
    maxGrowthFactor <-
      let maxValue = 10
       in option auto (long "max-growth-factor" <> metavar.int <> value maxValue <> help [i|The factor by which to allow the input term to grow before stopping. Defaults to #{maxValue}.|])
    inputFile <- inputFileArg
    pure CLI'TransformPhi{..}
  dataize = do
    rulesPath <- strOption (long "rules" <> short 'r' <> metavar.file <> help [i|#{fileMetavarName} with user-defined rules. Must be specified.|])
    inputFile <- inputFileArg
    outputFile <- outputFileOption
    recursive <- switch (long "recursive" <> help "Apply dataization + normalization recursively.")
    pure CLI'DataizePhi{..}

data CommandParserInfo = CommandParserInfo
  { metrics :: ParserInfo CLI
  , transform :: ParserInfo CLI
  , dataize :: ParserInfo CLI
  }

commandParserInfo :: CommandParserInfo
commandParserInfo =
  CommandParserInfo
    { metrics = info (CLI'MetricsPhi' <$> commandParser.metrics) (progDesc "Collect metrics for a PHI program.")
    , transform = info (CLI'TransformPhi' <$> commandParser.transform) (progDesc "Transform a PHI program.")
    , dataize = info (CLI'DataizePhi' <$> commandParser.dataize) (progDesc "Dataize a PHI program.")
    }

data CommandNames = CommandNames
  { transform :: String
  , metrics :: String
  , dataize :: String
  }

commandNames :: CommandNames
commandNames =
  CommandNames
    { transform = "transform"
    , metrics = "metrics"
    , dataize = "dataize"
    }

cli :: Parser CLI
cli =
  hsubparser
    ( command commandNames.transform commandParserInfo.transform
        <> command commandNames.metrics commandParserInfo.metrics
        <> command commandNames.dataize commandParserInfo.dataize
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

die :: (Exception e) => Optparse.Context -> e -> IO a
die parserContext exception = do
  handleParseResult . Failure $
    parserFailure pprefs cliOpts (ErrorMsg (show exception)) [parserContext]

newtype CLIException = CLIException String deriving (Show, Exception)

throw' :: String -> IO a
throw' = throw . CLIException

handle' :: Optparse.Context -> IO a -> IO a
handle' parserContext = Exception.handle (\(e :: SomeException) -> die parserContext e)

getFile :: Maybe FilePath -> IO (Maybe String)
getFile = \case
  Nothing -> pure Nothing
  Just file' ->
    doesFileExist file' >>= \case
      True -> pure (Just file')
      False -> throw' [i|File '#{file'}' does not exist.|]

getProgram :: Maybe FilePath -> IO Program
getProgram inputFile = do
  inputFile' <- getFile inputFile
  src <- maybe getContents' readFile inputFile'
  case parseProgram src of
    Left err -> throw' [i|An error occurred when parsing the input program: #{err}|]
    Right program -> pure program

getLoggers :: Maybe FilePath -> IO (String -> IO (), String -> IO ())
getLoggers outputFile = do
  handle <- maybe (pure stdout) (`openFile` WriteMode) outputFile
  pure
    ( \x -> hPutStrLn handle x >> hFlush handle
    , \x -> hPutStr handle x >> hFlush handle
    )

getParserContext :: String -> ParserInfo a -> Optparse.Context
getParserContext = Optparse.Context

-- >>> splitStringOn '.' "abra.cada.bra"
-- ["abra","cada","bra"]
--
-- >>> splitStringOn '.' ""
-- []
splitStringOn :: Char -> String -> [String]
splitStringOn sep = filter (/= [sep]) . groupBy (\a b -> a /= sep && b /= sep)

-- >>> flip getMetrics' (Just "a.b") "{⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ∅, d ↦ ⟦ φ ↦ ξ.ρ.c ⟧ ⟧, e ↦ ξ.b(c ↦ ⟦⟧).d ⟧ ⟧}"
-- Right (ProgramMetrics {bindingsByPathMetrics = Just (BindingsByPathMetrics {path = ["a","b"], bindingsMetrics = [BindingMetrics {name = "d", metrics = Metrics {dataless = 0, applications = 0, formations = 1, dispatches = 2}}]}), programMetrics = Metrics {dataless = 1, applications = 1, formations = 5, dispatches = 4}})
getMetrics' :: Program -> Maybe String -> Either String ProgramMetrics
getMetrics' program bindingsPath = do
  let metrics = getProgramMetrics program (splitStringOn '.' <$> bindingsPath)
  case metrics of
    Left path' ->
      ( case bindingsPath of
          Nothing ->
            let
              bindingsPath' = optionName.bindingsPath
              path'' = metavarName.path
             in
              Left
                [iii|
                  Impossible happened!
                  The option #{bindingsPath'} was not specified yet there were errors finding attributes by #{path''}.
                |]
          Just bindingsPath' ->
            Left
              [iii|
                Could not find bindings at path '#{bindingsPath'}'
                because an object at '#{intercalate "." path'}' is not a formation.
              |]
      )
    Right metrics' -> pure metrics'

getMetrics :: Maybe String -> Maybe FilePath -> IO ProgramMetrics
getMetrics bindingsPath inputFile = do
  program <- getProgram inputFile
  either throw' pure (getMetrics' program bindingsPath)

main :: IO ()
main = do
  opts <- customExecParser pprefs cliOpts
  let printAsProgramOrAsObject = \case
        Formation bindings' -> printTree $ Program bindings'
        x -> printTree x
  case opts of
    CLI'MetricsPhi' CLI'MetricsPhi{..} ->
      handle' (getParserContext commandNames.metrics commandParserInfo.metrics) do
        (logStrLn, _) <- getLoggers outputFile
        metrics <- getMetrics bindingsPath inputFile
        logStrLn $ encodeToJSONString metrics
    CLI'TransformPhi' CLI'TransformPhi{..} ->
      handle' (getParserContext commandNames.transform commandParserInfo.transform) do
        program' <- getProgram inputFile
        (logStrLn, logStr) <- getLoggers outputFile
        ruleSet <- parseRuleSetFromFile rulesPath
        unless (single || json) $ logStrLn ruleSet.title
        let Program bindings = program'
            uniqueResults
              | chain = applyRulesChainWith limits ctx (Formation bindings)
              | otherwise = pure <$> applyRulesWith limits ctx (Formation bindings)
             where
              limits = ApplicationLimits maxDepth (maxGrowthFactor * objectSize (Formation bindings))
              ctx = defaultContext (convertRule <$> ruleSet.rules) (Formation bindings)
            totalResults = length uniqueResults
        when (null uniqueResults || null (head uniqueResults)) (throw' [i|Could not normalize the program.|])
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
    CLI'DataizePhi' CLI'DataizePhi{..} -> do
      (logStrLn, _logStr) <- getLoggers outputFile
      program' <- getProgram inputFile
      ruleSet <- parseRuleSetFromFile rulesPath
      let (Program bindings) = program'
      let inputObject = Formation bindings
      let ctx = defaultContext (convertRule <$> ruleSet.rules) inputObject
      let dataize
            -- This should be moved to a separate subcommand
            | recursive = dataizeRecursively
            | otherwise = dataizeStep
      case dataize ctx inputObject of
        Left obj -> logStrLn (printAsProgramOrAsObject obj)
        Right (Bytes bytes) -> logStrLn bytes
