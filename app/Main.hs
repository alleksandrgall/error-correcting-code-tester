module Main where

import ExperimentExample
import Common.DB
import Database.Persist.Sqlite
import Database.Persist

main :: IO ()
main = do
  let plotDir = "D:/Thesis/error-correcting-code-tester/DB/Files/Experiment/TestExperiment/Plots/15_7/"
      expDir = "D:/Thesis/error-correcting-code-tester/DB/Files/Experiment/TestExperiment/Temp/"
      name = "name"
  runApp expDir plotDir expApp15_7
  return ()