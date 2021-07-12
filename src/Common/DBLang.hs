{-# LANGUAGE BlockArguments #-}
module Common.DBLang where

import Polysemy
import Polysemy.State
import qualified Common.DB as DB
import Experiment.Core
import Data.Time
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist as P
import Database.Persist.TH
import Database.Persist.Sqlite
import PraseArgs

data ExperimentStats = ExperimentStats
  {
    logFile :: FilePath,
    timeOfExp :: UTCTime,
    runTimeSec :: Double
  }

data ExperimentRes = ExperimentRes
  {
  experimentConfig :: Experiment,
  experimentStats :: ExperimentStats,
  plotPaths :: [FilePath]
  }

data DBLang s m a where
  GetPlotsFromExperiment :: String -> DBLang s m [String]
  GetStatsExperiment :: String -> DBLang s m [ExperimentStats]
  WriteExperiment :: ExperimentRes -> DBLang s m ()
