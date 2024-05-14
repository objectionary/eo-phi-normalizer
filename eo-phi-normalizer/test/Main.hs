module Main where

import Main.Utf8
import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = withUtf8 $ hspecWith defaultConfig Spec.spec
