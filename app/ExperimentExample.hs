module ExperimentExample where

import Common.App
import Experiment.Core
import HexCalc
import Data.Monoid
import Common.Plot
import Polysemy
import Polysemy.Bundle
import Polysemy.State
import Logger
import Common.ChainProcess
import Common.Plot
import Common.App
import Common.Calculations
import Control.Monad (replicateM_)

runApp :: FilePath -> FilePath -> Sem (App [Bool] [Bool] ': '[]) () -> IO ()
runApp expDir plotDir = runM
  . interpretChartLangCairo plotDir
  . runLoggerToStdOut
  . interpreterCalcBool
  . evalState ([], "")
  . interpretChainProcessHandle expDir
  . runBundle
  . rewrite (\(App bundle) -> bundle).
    raiseUnder @(Embed IO)

hammingCode :: Color -> [Bool] -> Int -> Int -> Int -> Sem (App [Bool] [Bool] ': '[]) ([Bool], PlotDouble)
hammingCode col mes n k i = do
  setInputA mes
  runProgramA (exampleCoder n k)
  encoded1 <- getInputA
  decodedInfo <- whileMonoidM (<0.01)  (\p -> do
    setInputA encoded1
    runProgramA (evenNoise p)
    outputFileTempA
    replicateM_ i $ runProgramA (exampleDecoder n k)
    decoded <- getInputA
    return (p + 0.0001, [((decoded, k), p*fromIntegral n)])
    ) 0.001
  points1 <- errorInChannelToErrorInEncodedChannelWordA True mes decodedInfo
  return $ (,) encoded1 $ PlotDouble points1 ("Каскад из " ++ show i ++ " кодов Хэмминга (" ++ show n ++ ", " ++ show k ++ ")") col

expApp :: Sem (App [Bool] [Bool] ': '[]) ()
expApp = do
  inputFileA stdinput
  inputMes <- getInput
  (encoded1, plot1) <- hammingCode K inputMes 7 4 1
  chartOutAsJSONA (defaultChartOptions plot1)  (plotName plot1)
  (encoded2, plot2) <- hammingCode B encoded1 7 4 2
  chartOutAsJSONA (defaultChartOptions plot2)  (plotName plot2)  
  (encoded3, plot3) <- hammingCode R encoded2 7 4 3
  chartOutAsJSONA (defaultChartOptions plot3)  (plotName plot3)
  (encoded4, plot4) <- hammingCode Y encoded2 7 4 4
  chartOutAsJSONA (defaultChartOptions plot4)  (plotName plot4)
  (encoded5, plot5) <- hammingCode G encoded2 7 4 5
  chartOutAsJSONA (defaultChartOptions plot5)  (plotName plot5)
  (encoded6, plot6) <- hammingCode M encoded2 7 4 6
  chartOutAsJSONA (defaultChartOptions plot6)  (plotName plot6)

  makeChartA (ChartParamsDouble [plot1, plot2, plot3, plot4, plot5, plot6] "Сравнение эффективности каскадов кодов Хэмминга 7 4"
    (Linear, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Linear, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
    PNG
  makeChartA (ChartParamsDouble [plot1, plot2] "Сравнение эффективности каскадов кодов Хэмминга 7 4"
    (Linear, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Logarithmic, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
    PNG
  makeChartA (ChartParamsDouble [plot1, plot2] "Сравнение эффективности каскадов кодов Хэмминга 7 4"
      (Logarithmic, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Logarithmic, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
      PNG
  makeChartA (ChartParamsDouble [plot1, plot2] "Сравнение эффективности каскадов кодов Хэмминга 7 4"
      (Logarithmic, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Linear, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
      PNG
  return ()
--
