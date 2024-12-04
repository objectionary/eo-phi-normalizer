{- FOURMOLU_DISABLE -}
-- The MIT License (MIT)

-- Copyright (c) 2016-2024 Objectionary.com

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.
{- FOURMOLU_ENABLE -}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.Pipeline.Dataize.PrintConfigs where

import Data.Functor ((<&>))
import Data.List (stripPrefix)
import Data.Maybe
import Language.EO.Phi.Pipeline.Config
import PyF (fmt)

printDataizeConfigs :: PipelineConfig -> [String] -> Bool -> IO ()
printDataizeConfigs pipelineConfig phiPrefixes singleLine = do
  let args =
        pipelineConfig.testSets <&> \testSet ->
          let phiInitial = testSet.phi.initial
              phi =
                case catMaybes [stripPrefix prefix phiInitial | prefix <- phiPrefixes] of
                  [] -> phiInitial
                  x : _ -> x
              atoms = case testSet.atoms of
                Nothing -> ""
                Just (AtomsSet{..}) ->
                  let enable' = unwords . ((\atom -> [fmt|--enable-atom {atom}|]) <$>) <$> enable
                      disable' = unwords . ((\atom -> [fmt|--disable-atom {atom}|]) <$>) <$> disable
                   in unwords (catMaybes [enable', disable'])
              enableTestSet
                | fromMaybe True testSet.enable = "true"
                | otherwise = "false"
           in [fmt|'{{"atoms": "{atoms}", "phi": "{phi}", "enable": "{enableTestSet}"}}'|]
  putStrLn ((if singleLine then unwords else unlines) args)
