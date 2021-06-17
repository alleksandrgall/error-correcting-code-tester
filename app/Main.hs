module Main where

import Common.DB
import Database.Persist
import Database.Persist.Sqlite

main :: IO ()
main = do
  let plotDir = "D:/Thesis/error-correcting-code-tester/DB/Files/Experiment/TestExperiment/Plots/Valid/"
      expDir = "D:/Thesis/error-correcting-code-tester/DB/Files/Experiment/TestExperiment/Temp/"
      name = "name"
  paths <- runSqlite "DB/test.dp" $ do
    runMigration migrateAll
    exps <- selectKeysList [ExperimentExperimentName ==. name] []
    plots <- concat <$> (mapM (\e -> selectList [PlotExperimentId ==. e] []) exps)
    return $ map (\p -> plotPath . entityVal $ p) plots
  return ()