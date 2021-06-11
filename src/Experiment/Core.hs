module Experiment.Core where

import Control.Lens
import Language.Haskell.TH.Lens
import Common.Plot (Color(..), AxesType(..))

--Список графиков которые могут получится в результате:
--1(2). График доли ошибки после декодирования[ось Y] в бите(слове) для одной/нескольких пар кодер-декодер с общим генератором ошибок
--диапазон параметров и форматная строка параметров, а так же другая информация задается отдельно для каждой пары кодер-декодер
--(Как задавать ось X???? Варианты: число k с поправкой на размерность (найти поправку)
--(!!!!Как считать число k??? Варианты: попросить пользователя предоставить формулу для k от n!!!! Не решено пока
--(В каком виде предоставлять формулу??? Варианты: )))
--Как флаг можно включить отображение доли ошибки от источника шума(прямая пар. оси X) и логарифмическую шкалу
--2(3). График доли ошибки после декодирования[ось Y] в бите(слове) для одной/нескольких пар кодер декодер с фиксированными параметрами
-- и одного генератора ошибок с изменяемой частотой ошибки
--вероятностью ошибки[ось X]
--Для кодов разных размерностей будет компенсация
--Для кодеров/декодеров
newtype ExperimentDir = ExperimentDir { path :: FilePath }

data ErrorCountType = Word | Byte

data Iterator a = PIter | NoIter

data ProgramType = Coder | Decoder | Noise deriving (Eq, Show)
data InbuildNoise = Even Double | Burst Double (Int, Int)

data ProgramInfo = ProgramInfo {
  programType :: ProgramType,
  pathToProgram :: FilePath,
  formatStr :: String,
  env :: Maybe FilePath
  } | Inbuild InbuildNoise 
 

--data NoiseSettings = NoiseSettings {noiseInfo :: ProgramInfo} | Even | Burst
--data NoiseCascadeSettings = NoiseCascadeSettings NoiseSettings NoiseCascadeSettings | NoiseCascadeSettingsLast NoiseSettings
--
--newtype CoderSettings = CoderSettings {coderInfo :: ProgramInfo}
--data CoderCascadeSettings = CoderCascadeSettings CoderSettings CoderCascadeSettings | CoderCascadeSettingsLast CoderSettings
--
--newtype DecoderSettings = DecoderSettings {decoderInfo :: ProgramInfo}
--data DecoderCascadeSettings = DecoderCascadeSettings DecoderSettings DecoderCascadeSettings | DecoderCascadeSettingsLast DecoderSettings
--
--data PlotSettings = PlotSettings {
--  settingsCoder :: CoderCascadeSettings,
--  settingsNoise :: NoiseCascadeSettings,
--  settingsDecoder :: DecoderCascadeSettings,
--  settingsColor :: Color,
--  settingsName :: String
--}
--data ChartSettings = ChartSettings {
--  plotList :: [PlotSettings],
--  settingsLegend :: String,
--  settingsXAxis :: (AxesType, String),
--  settingsYAxis :: (AxesType, String)
--}
--
--data ExperimentConfig = ExperimentConfig {
--  experimentDir :: ExperimentDir,
--  chartsToBuild :: ChartSettings
--}