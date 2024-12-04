{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.EO.Locale where

import Control.Exception (Exception (..), SomeException, catch)
import Main.Utf8 (withUtf8)
import System.Exit (ExitCode (..), exitWith)
import System.IO.CodePage (withCP65001)

withCorrectLocale :: IO a -> IO a
withCorrectLocale act = do
  let withCorrectLocale' = withCP65001 . withUtf8
  withCorrectLocale' act
    `catch` ( \(x :: SomeException) ->
                withCorrectLocale' do
                  putStrLn (displayException x)
                  exitWith (ExitFailure 1)
            )
