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
import Data.Monoid

newtype App s i m a = App (Bundle '[ChainProcess s, Calculation i, Logger, ChartLang] m a)

inputFileA ::  forall s r i. Member (App s i) r => FilePath -> Sem r ()
inputFileA fp = do
 transform (App @s @i  . injBundle @(ChainProcess s)) (inputFile fp)
 transform (App @s @i . injBundle @(Logger)) (logInfo $ "File " ++ fp ++ " set as input")
 
outputFileA :: forall r s  i. Member (App s i) r => FilePath -> Sem r ()
outputFileA fp = do
  transform (App @s @i . injBundle @(ChainProcess s)) (inputFile fp)
  transform (App @s @i . injBundle @Logger) (logInfo $ "File " ++ fp ++ " was outputed")

outputFileTempA :: forall r s  i. Member (App s i) r => Sem r FilePath
outputFileTempA = do
  fp <- transform (App @s @i . injBundle @(ChainProcess s)) (outputFileTemp)
  transform (App @s @i . injBundle @Logger) (logInfo $ "File " ++ fp ++ " was outputed")
  return fp
  


errorInChannelToErrorInEncodedChannelWordA :: forall r s i . Member (App s i) r => Bool -> i -> [((i, Int), Double)] -> Sem r [(Double, Double)]
errorInChannelToErrorInEncodedChannelWordA compensationForDimFlag mes decodeds = do
  res <- transform (App @s @i. injBundle @(Calculation i)) (errorInChannelToErrorInEncodedChannelWord compensationForDimFlag mes decodeds )
  transform (App @s @i. injBundle @(Logger)) $ logInfo $ "Calculated error rate with compensation flag: " ++ show compensationForDimFlag
  return res
  
getInputA :: forall r s  i. Member (App s i) r => Sem r s
getInputA = do
  transform (App @s @i . injBundle @(ChainProcess s)) (getInput)

setInputA :: forall r s  i. Member (App s i) r => s -> Sem r ()
setInputA h = do
  transform (App @s @i . injBundle @(ChainProcess s)) (setInput h)
  transform (App @s @i . injBundle @Logger) (logInfo "New input was set")
  
runProgramA :: forall r s  i. Member (App s i) r => ProgramInfo -> Sem r ()
runProgramA pi = do
  transform (App @s @i . injBundle @(ChainProcess s)) (runProgram pi)
  transform (App @s @i . injBundle @Logger) (logInfo $ "Executable at path " ++ pathToProgram pi ++ " with arguments " ++ formatStr pi ++ " successfully launched")
  
makeChartA :: forall r s  i. Member (App s i) r => ChartParams -> FileFormat -> Sem r FilePath
makeChartA cp ff = do
  fp <- transform (App @s @i . injBundle @ChartLang) (makeChart cp ff)
  transform (App @s @i . injBundle @Logger) $ logInfo $ "Chart with name " ++ legend cp ++ " to " ++ fp ++ " with plots:\n " ++ foldl (\str p -> str ++ "Name " ++ plotName p ++ " color " ++ (show . color $ p) ++ "\n") "" (plotsDouble cp)
  return fp 
  
chartOutAsJSONA :: forall r s  i. Member (App s i) r => ChartParams -> FilePath -> Sem r ()
chartOutAsJSONA cp fp = do
  transform (App @s @i . injBundle @ChartLang) $ chartOutAsJSON cp fp
  transform (App @s @i . injBundle @Logger) $ logInfo $ "Chart info " ++ legend cp ++ " written as JSON to " ++ fp


  