{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.DB where

import Polysemy
import Polysemy.State
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time as T


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Message
  type String
  path String
Experiment
  experimentName String
  messageId MessageId
  logFilePath String
  experimentConfigPath String
  timeOfExp UTCTime default=CURRENT_TIME
  runTimeSecs Double
Plot
  title String Maybe
  path String
  experimentId ExperimentId
Program
  name String Maybe
  path String
  envPath String Maybe
  formatString String
NoiseCascade
  name String Maybe
Noise
  name String Maybe
  programId ProgramId
  cascadeOrder Int default=0
  noiseCascadeId NoiseCascadeId Maybe
CoderCascade
  name String Maybe
Coder
  name String Maybe
  programId ProgramId
  cascadeOrder Int default=0
  coderCascadeId CoderCascadeId Maybe    
DecoderCascade
  name String Maybe
Decoder
  name String Maybe
  programId ProgramId
  cascadeOrder Int default=0
  decoderCascadeId DecoderCascadeId Maybe 
CoderDecoder
  coderCascadeId CoderCascadeId
  decoderCascadeId DecoderCascadeId
Channel
  noiseCascadeId NoiseCascadeId
  coderDecoderId CoderDecoderId
ChannelExperiment
  channelId ChannelId
  coderRuntimeSecs Double
  decoderRuntimeSecs Double
  noiseRuntimeSecs Double
  errorRateBit Double
  errorRateWord Double
  experimentId ExperimentId
|]