module Main where

import ExperimentExample

main :: IO ()
main = do
  let plotDir = "D:/Thesis/error-correcting-code-tester/DB/Files/Experiment/TestExperiment/Plots/GuilbertInt/"
      expDir = "D:/Thesis/error-correcting-code-tester/DB/Files/Experiment/TestExperiment/Temp/"
      name = "name"
  runApp expDir plotDir expAppPack
  return ()