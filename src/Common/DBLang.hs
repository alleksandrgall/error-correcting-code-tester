module Common.DBLang where

import Polysemy
import Polysemy.State
import Common.DB as DB
import Experiment.Core
import Data.Time
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Database.Persist as P
import Database.Persist.TH
import Database.Persist.Sqlite

data ExperimentStats = ExperimentStats
  {
    logFile :: FilePath,
    timeOfExp :: UTCTime,
    runTimeSec :: Double
  }

--type Transaction m a = ReaderT SqlBackend (NoLoggingT (ResourceT m)) a
--
--data DBLang s m a where
--  GetPlotsFromExperiment :: ExperimentId -> DBLang s m (Transaction s [FilePath])
--  GetCoderDecoderFromExperiment :: ExperimentId -> DBLang s m (Transaction s [([CoderSettings], [DecoderSettings])])
--  GetNoiseFromExperiment :: ExperimentId -> DBLang s m (Transaction s [[NoiseSettings]])
--  GetExperimentStats :: ExperimentId -> DBLang s m (Transaction s ExperimentStats)
  
