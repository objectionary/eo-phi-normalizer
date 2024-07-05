{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.EO.Phi.Pipeline.Dataize.PrintConfigs where

import Control.Monad (forM_)
import Data.List (stripPrefix)
import Data.Maybe
import Language.EO.Phi.Pipeline.Config
import PyF (fmt)

printDataizeConfigs :: PipelineConfig -> [String] -> IO ()
printDataizeConfigs pipelineConfig phiPrefixes = do
  forM_ pipelineConfig.testSets $ \testSet -> do
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
    putStrLn [fmt|{{"atoms": "{atoms}", "phi": "{phi}"}}|]
