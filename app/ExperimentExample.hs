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
import Data.List
import Common.ChainProcess
import Common.Plot
import Common.App
import Common.Calculations
import Control.Monad
import Math.Combinatorics.Exact.Factorial (factorial)
--
runApp :: FilePath -> FilePath -> Sem (App (Inp [Bool]) [Bool] ': '[]) a -> IO a
runApp expDir plotDir = runM
  . interpretChartLangCairo plotDir
  . runLoggerToStdOut
  . interpreterCalcBool
  . evalState (([], []), "")
  . interpretChainProcessHandle expDir
  . runBundle
  . rewrite (\(App bundle) -> bundle).
    raiseUnder @(Embed IO)

data UI = Un | In

ifInterleave :: UI -> (Maybe ProgramInfo) -> Sem (App (Inp [Bool]) [Bool] ': '[]) ()
ifInterleave _ Nothing = pure ()
ifInterleave In (Just (Inbuild (Interleaver n m))) = do
  runProgramA (interleaveBlock n m)
ifInterleave Un (Just (Inbuild (Interleaver n m))) = do
  runProgramA (unInterleaveBlock n m)


hammingCode :: (Maybe ProgramInfo) -> ProgramInfo -> Bool -> Color -> Inp [Bool] -> Int -> Int -> Int -> Sem (App (Inp [Bool]) [Bool] ': '[]) PlotDouble
hammingCode terleaver (Inbuild err) comp col mes n k i = do
  runProgramA (exampleCoder n k)
  encoded1 <- getInputA
  ifInterleave In terleaver
  encodedIL <- getInputA
  decodedInfo <- whileMonoidM (<0.033)  (\p -> do
    setInputA encodedIL
    case err of
      (Even _) -> runProgramA (evenNoise p)
      (Burst _ (b1, b2)) -> runProgramA (burstNoise p (b1, b2))
      (Guilbert p' q _) -> runProgramA (guilberNoise p' q p)
    ifInterleave Un terleaver
    mes' <- fst <$> getInputA
    pWord <- countWordErrorRateA (fst encoded1) (mes', n)
    replicateM_ i $ runProgramA (exampleDecoder n k)
    decoded <- getInputA
    logInfoA $ "----" ++ show (pWord) ++ " || " ++ (show . getErrorRateWordBool k (inpMes mes) $ (inpMes decoded)) ++ "----"
    return (p + 0.0001, [((inpMes decoded, k), pWord)])
    ) 0.001
  points1 <- errorInChannelToErrorInEncodedChannelWordA comp (inpMes mes) decodedInfo
  setInputA encoded1
  return $ PlotDouble points1 ("Код Хэмминга (" ++ show n ++ ", " ++ show k ++ ")" ++
    (if comp then " с компенсацией размерности " else "") ++
    (if terleaver == Nothing then "" else let Just (Inbuild (Interleaver n m)) = terleaver in
      " с блочным перемежителем " ++ show n ++ " " ++ show m) ++ (case err of
          (Even p) -> " и одиночными ошибками"
          (Burst p (b1, b2)) -> " и пакетными ошибками длины от " ++ show b1 ++ " до " ++ show b2
          (Guilbert p q pp)  -> " и гильбертовой моделью ошибок q=" ++ show q ++ "; p=" ++ show p) ++ "\n") col

hammingCode' :: String -> (Maybe ProgramInfo) -> ProgramInfo -> Bool -> Color -> Inp [Bool] -> Int -> Int -> Int -> Sem (App (Inp [Bool]) [Bool] ': '[]) PlotDouble
hammingCode' str terleaver (Inbuild err) comp col mes n k i = do
  runProgramA (exampleCoder n k)
  encoded1 <- getInputA
  ifInterleave In terleaver
  encodedIL <- getInputA
  decodedInfo <- whileMonoidM (<0.01)  (\p -> do
    setInputA encodedIL
    case err of
      (Even _) -> runProgramA (evenNoise p)
      (Burst _ (b1, b2)) -> runProgramA (burstNoise p (b1, b2))
      (Guilbert p' q _) -> runProgramA (guilberNoise p' q p)
    ifInterleave Un terleaver
    mes' <- fst <$> getInputA
    pWord <- countWordErrorRateA (fst encoded1) (mes', n)
    replicateM_ i $ runProgramA (exampleDecoder n k)
    decoded <- getInputA
    logInfoA $ "----" ++ show (pWord) ++ " || " ++ (show . getErrorRateWordBool k (inpMes mes) $ (inpMes decoded)) ++ "----"
    return (p + 0.0001, [((inpMes decoded, k), pWord)])
    ) 0.001
  points1 <- errorInChannelToErrorInEncodedChannelWordA comp (inpMes mes) decodedInfo
  setInputA encoded1
  return $ PlotDouble points1 str col


hammingCodeValid :: Color ->  Int -> Int -> Sem (App (Inp [Bool]) [Bool] ': '[]) PlotDouble
hammingCodeValid col n k = do
  let fact ::  Int -> Double
      fact num = fromIntegral (factorial $ num :: Int)
      slog :: Double -> Int -> Double
      slog p0 j = ((fact n)/((fact (n - j)) * (fact j))) 
            * ((p0 ** (fromIntegral j)) * ((1 - p0) ** (fromIntegral $ n-j)))
  points1 <- whileMonoidM (<0.033)  (\p -> do
    return (p + 0.0001, [(p*(fromIntegral n), sum . map (slog p) $ [2..n])])
    ) 0.001
  return $ PlotDouble points1 ("Теоритическая вероятность неверного декодирования слова код Хэмминга (" ++ show n ++ ", " ++ show k ++ ")") col


expAppValid :: Sem (App (Inp [Bool]) [Bool] ': '[]) ()
expAppValid = do
  inputFileA stdinput
  inputMes <-getInputA
  p1 <- hammingCodeValid R 7 4
  p2 <- hammingCode Nothing (Inbuild $ Even 0) False B inputMes 7 4 1
  makeChartA (ChartParamsDouble [p1, p2] "Валидация программы сравнением теоритических и фактических долей неверно декодированных слов"
               (Linear, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Linear, "Доля слов переданных с ошибкой в канале с защитой от ошибок")) PNG
  return ()

--expAppComp :: Sem (App (Inp [Bool]) [Bool] ': '[]) ()
--expAppComp = do
--  inputFileA stdinput
--  inputMes <- getInputA
--  p1 <- hammingCode Nothing (Inbuild $ Even 0) False B input

expApp :: Sem (App (Inp [Bool]) [Bool] ': '[]) ()
expApp = do
  inputFileA stdinput
  inputMes <- getInputA
  runProgramA (exampleCoder 7 4)
  plots <- zipWithM (\c cascNum -> hammingCode Nothing (Inbuild $ Even 0.001) True c inputMes 7 4 cascNum) [K, B, R, Y, G, M] [2, 3, 4, 5, 6, 7]
  let c1 = (ChartParamsDouble plots "Сравнение эффективности каскадов кодов Хэмминга 7 4"
        (Linear, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Linear, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
  makeChartA c1 PNG
  chartOutAsJSONA c1 "c1"
  let c2 = (ChartParamsDouble plots "Сравнение эффективности каскадов кодов Хэмминга 7 4"
          (Linear, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Logarithmic, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
  makeChartA c2 PNG
  chartOutAsJSONA c2 "c2"
  let c3 = (ChartParamsDouble plots "Сравнение эффективности каскадов кодов Хэмминга 7 4"
          (Logarithmic, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Linear, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
  makeChartA c3 PNG
  chartOutAsJSONA c3 "c3"

  let c4 = (ChartParamsDouble plots "Сравнение эффективности каскадов кодов Хэмминга 7 4"
          (Logarithmic, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Logarithmic, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))

  makeChartA c4 PNG
  chartOutAsJSONA c4 "c4"
  return ()

expApp15_7 :: Sem (App (Inp [Bool]) [Bool] ': '[]) ()
expApp15_7 = do
  inputFileA stdinput
  inputMes <- getInputA
  p1 <- hammingCode' ("(7, 4) код Хэмминга c компенсацией на размерность") Nothing (Inbuild $ Even 0) True B inputMes 7 4 1
  setInputA inputMes
  p2 <- hammingCode' ("(7, 4) код Хэмминга\n") Nothing (Inbuild $ Even 0) False B inputMes 7 4 1
  setInputA inputMes
  p3 <- hammingCode' ("(15, 11) код Хэмминга с компенсацией на размерность") Nothing (Inbuild $ Even 0) True R inputMes 15 11 1
  setInputA inputMes
  p4 <- hammingCode' ("(15, 11) код Хэмминга") Nothing (Inbuild $ Even 0) False R inputMes 15 11 1
  let p1Max = fst. last . points $ p1
      points3 = points p3
      points4 = points p4
      p3' = p3 {points = takeWhile (\(p, _) -> p < p1Max) points3}
      p4' = p4 {points = takeWhile (\(p, _) -> p < p1Max) points4}
  makeChartA (ChartParamsDouble [p1, p2, p3', p4'] "Тестовый эксперимент"
     (Linear, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Linear, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
      PNG
  return ()

expAppBurstVsEven :: Sem (App (Inp [Bool]) [Bool] ': '[]) ()
expAppBurstVsEven = do
  inputFileA stdinput
  inputMes <- getInputA
  p1 <- hammingCode Nothing (Inbuild $ Even 0) False B inputMes 15 11 1
  setInputA inputMes
  p2 <- hammingCode Nothing (Inbuild $ Guilbert 0.1 0.1 0) False R inputMes 15 11 1
  makeChartA
     (ChartParamsDouble [p1, p2] "Сравнение работы кода Хэмминга для пакетных и одиночных ошибок"
     (Linear, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Linear, "Доля слов переданных с ошибкой в канале с защитой от ошибок"))
      PNG
  return ()

expAppPack :: Sem (App (Inp [Bool]) [Bool] ': '[]) ()
expAppPack = do
  inputFileA stdinput
  inputMes <- getInputA
  plotsUnInterleaved <-
    zipWithM (\c cascNum -> hammingCode Nothing (Inbuild $ Guilbert 0.1 0.1 0.001) False c inputMes 7 4 cascNum)
       (replicate 4 R) [1]
  setInputA inputMes
  plotsInterleaved1 <-
    zipWithM (\c cascNum -> hammingCode (stdInterleaver (cascNum*round(7/4)*7) (cascNum*round(7/4)*7)) (Inbuild $ Guilbert 0.1 0.1 0.001)
      False c inputMes 7 4 cascNum)
        (replicate 4 B) [1]

  let plotlls = concat . zipWith3 (\num p1 p2  ->
            [ChartParamsDouble [p1, p2]
              ("Эффективность перемежителя для каскада из кода Хэмминга 7 4 в канале с Гильбертовой моделью ошибок")
              (Linear, "Доля слов переданных с ошибкой в канале без защиты от ошибок") (Linear, "Доля слов переданных с ошибкой в канале с защитой от ошибок")
            ]) [1,2] plotsUnInterleaved $ plotsInterleaved1

  zipWithM_ (\n p -> makeChartA p PNG >> chartOutAsJSONA p (show n)) [1,2..]  plotlls