{-# LANGUAGE BlockArguments #-}
module Common.App where

import Polysemy.Bundle
import Polysemy
import Common.ChainProcess
import Common.Logger
import Common.Calculations
import Experiment.Core
import Common.Plot
import System.IO
import Common.Plot

newtype App s m a = App (Bundle '[ChainProcess s, Calculation, Logger, ChartLang] m a)

inputFileA ::  forall s r . Member (App s) r => FilePath -> Sem r ()
inputFileA fp = do
 transform (App @s . injBundle @(ChainProcess s)) (inputFile fp)
 transform (App @s . injBundle @(Logger)) (logInfo $ "File " ++ fp ++ " set as input")
 
outputFileA :: forall r s . Member (App s) r => FilePath -> Sem r ()
outputFileA fp = do
  transform (App @s . injBundle @(ChainProcess s)) (inputFile fp)
  transform (App @s . injBundle @Logger) (logInfo $ "File " ++ fp ++ " was outputed")

outputFileTempA :: forall r s . Member (App s) r => Sem r FilePath
outputFileTempA = do
  fp <- transform (App @s . injBundle @(ChainProcess s)) (outputFileTemp)
  transform (App @s . injBundle @Logger) (logInfo $ "File " ++ fp ++ " was outputed")
  return fp
 
errorInChannelToErrorInEncodedChannelWordA :: forall r s . Member (App s) r => Bool -> FilePath -> [(FilePath, Int, Int)] -> [Double] -> Sem r [(Double, Double)]
errorInChannelToErrorInEncodedChannelWordA compensationForDimFlag mes decodeds probErrs = do
  transform (App @s . injBundle @(Calculation)) (errorInChannelToErrorInEncodedChannelWord compensationForDimFlag mes decodeds probErrs)

setInputA :: forall r s . Member (App s) r => s -> Sem r ()
setInputA h = do
  transform (App @s . injBundle @(ChainProcess s)) (setInput h)
  transform (App @s . injBundle @Logger) (logInfo "New input was set")
  
runProgramA :: forall r s . Member (App s) r => ProgramInfo -> Sem r ()
runProgramA pi = do
  transform (App @s . injBundle @(ChainProcess s)) (runProgram pi)
  transform (App @s . injBundle @Logger) (logInfo $ "Executable at path " ++ pathToProgram pi ++ " with arguments " ++ formatStr pi ++ " successfully launched")
  
makeChartA :: forall r s . Member (App s) r => ChartParams -> FileFormat -> Sem r FilePath
makeChartA cp ff = do
  fp <- transform (App @s . injBundle @ChartLang) (makeChart cp ff)
  transform (App @s . injBundle @Logger) $ logInfo $ "Chart with name " ++ legend cp ++ " to " ++ fp ++ " with plots:\n " ++ foldl (\str p -> str ++ "Name " ++ plotName p ++ " color " ++ (show . color $ p) ++ "\n") "" (plotsDouble cp)
  return fp 
  