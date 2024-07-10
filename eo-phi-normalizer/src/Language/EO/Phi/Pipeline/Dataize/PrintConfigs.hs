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
           in [fmt|'{{"atoms": "{atoms}", "phi": "{phi}"}}'|]
  putStrLn ((if singleLine then unwords else unlines) args)
