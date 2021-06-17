module PraseArgs where

import Options.Applicative

data DataType = RawBites | RawBitesByWord | CMDArgByWord

data Prog = Prog {
  _path :: String,
  _formatStr :: String,
  _inputType :: DataType
}

rawBytesParser :: Parser DataType
rawBytesParser = flag' RawBites
  (long "rawBytes" <>
  short 'b' <>
  help "Input message via pip")

rawBytesByWordParser :: Parser DataType
rawBytesByWordParser = flag' RawBitesByWord
  (long "rawBytesByWord" <>
  short 'w' <>
  help "Input message via pipe word by word")

cmdArgsByWordParser :: Parser DataType
cmdArgsByWordParser = flag' CMDArgByWord
  (long "cmdArgs" <>
  short 'c' <>
  help "Input message in cmd as Hex")

dataTypeParser :: Parser DataType
dataTypeParser = cmdArgsByWordParser <|> rawBytesByWordParser <|> rawBytesParser

progParser :: Parser Prog
progParser = Prog
  <$> strOption
    (long "path" <>
     short 'p' <>
     metavar "PATH" <>
     help "Path to program")
  <*> strOption
    (long "form" <>
     short 'f' <>
     metavar "FORMAT" <>
     help "Format string")
  <*> dataTypeParser

data Coder = Coder [Prog]
data Noise = Noise [Prog]
data Decoder = Decoder [Prog]

data Channel = Channel {
  _coder :: Coder,
  _noise :: Noise,
  _decoder :: Decoder
}

coderParser :: Parser Coder
coderParser = Coder <$> some progParser

decoderParser :: Parser Decoder
decoderParser = Decoder <$> some progParser

noiseParser :: Parser Noise
noiseParser = Noise <$> some progParser

channelParser :: Parser Channel
channelParser = Channel
  <$> coderParser
  <*> noiseParser
  <*> decoderParser

data PlotType = ByWord | ByBit

byWordParser :: Parser PlotType
byWordParser = flag' ByWord
  (long "wordEr" <>
   short 'o' <>
   help "Plot error by word")

byBitParser :: Parser PlotType
byBitParser =flag' ByBit
  (long "bitEr" <>
   short 'i' <>
   help "Plot error by bit")

plotTypeParser = byWordParser <|> byBitParser

type Range = (String, String)

rangeParser :: Parser (String, String)
rangeParser = (,) <$> (argument str (metavar "LOW" <> help "low")) <*> (argument str (metavar "HIGH" <> help "high"))

data Experiment = Experiment {
  channels :: [Channel],
  plotType :: PlotType,
  range :: Range,
  name :: Maybe String
}

experimentParser :: Parser Experiment
experimentParser = Experiment <$> some channelParser <*> plotTypeParser <*> rangeParser <*> pure Nothing

data GetBy = ByName String | ByTime String String

byNameParser :: Parser GetBy
byNameParser = ByName <$>
  strOption (
    long "name" <>
    short 'n' <>
    metavar "NAME" <>
    help "Name of experiment"
  )

byTimeParser :: Parser GetBy
byTimeParser = ByTime <$>
  strOption (
    long "from" <>
    metavar "D1" <>
    help "From date.."
  )
  <*> strOption (
    long "to" <>
    metavar "D2" <>
    help "..to date"
  )

getByParser = byNameParser <|> byTimeParser

data GetWhat = Plots | Runtime
plotsParser = flag' Plots (long "plots" <> help "Get plots")
runtimeParser = flag' Runtime (long "runtime" <> help "Get runtimes")
getWhatParser = plotsParser <|> runtimeParser

data Get = Get {
  getBy :: GetBy,
  getWhat :: GetWhat
}

getParser = Get <$> getByParser <*> getWhatParser

data Command = ExperimentC Experiment | GetC Get
data Options = Options {
  optCommand :: Command
}

optionsParser :: Parser Options
optionsParser = Options <$> hsubparser ((command "run" (info (ExperimentC <$> experimentParser) (progDesc "Run Experiment"))) <> (command "get" (info (GetC <$> getParser) (progDesc "Get from db"))))

--parserExperiment :: Parser Experiment
--parserExperiment = Experiment <$>
--
--experimentInput :: Parser Opts
--experimentInput = OptsE <$> undefined