{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString as B
import Experiment.Core
import HexCalc
import Common.ChainProcess

main :: IO ()
main = do
  bs <- B.readFile stdinput
  let bools = readBytestringAsBoolList bs
      hexes = boolsToHexList 4 bools
      bools' = hexListToBools 4 hexes
      boolsNoComp' = hexListToBoolsNoComp hexes
  print $ bools == bools' 
  boolsEncoded <- wordByWord "D:/hamming-codec/build/bin/example_encode.exe" (4, 7) bools
  B.writeFile "D:/Thesis/error-correcting-code-tester/DB/testEncoded"  (boolListToBytestring boolsEncoded)
  B.writeFile "D:/Thesis/error-correcting-code-tester/DB/test"  (boolListToBytestring bools)
  B.writeFile "D:/Thesis/error-correcting-code-tester/DB/test1" (boolListToBytestring bools')
  B.writeFile "D:/Thesis/error-correcting-code-tester/DB/test2" (boolListToBytestring boolsNoComp')
  print 4
  return ()
--  let expDir = "D:/Thesis/error-correcting-code-tester/DB/Files/Experiment/TestExperiment/"
--     plotDir = "D:/Thesis/error-correcting-code-tester/DB/Files/Plots/"
-- runApp plotDir expDir expApp
   --let x = runBundle . rewrite (\(App bundle) -> bundle) . raiseUnder @(Embed IO) $ expApp
--  start <- C.getCurrentTime
--  end <- C.getCurrentTime
--  print $ diffUTCTime end start
--  print countB
--  points' <- makeRandomPoints
--  let pp = PlotParams points' "test plot" B
--      cp = ChartParams [pp] "test chart" (Linear, "x") (Linear, "y")
--  runM
--  . runState (ExperimentDir "D:/Thesis/error-correcting-code-tester/temp")
--  . interpretChartLangCairo
--  $ makeChart cp PNG
--  h <- openFile "D:/Thesis/error-correcting-code-tester/temp/img.png" ReadMode
--  let x =
--      runLoggerToStdOut
--    . raiseUnder
--    . runState 0
--    . interpretChainProcessTest
--    . addLogging'
--    . (raiseUnder :: Sem (ChainProcess Int ': r) a -> Sem (ChainProcess Int ': Logger ': r) a)
--    $ chainProg3' "" "" "" 0
--  res <- runM  x
--  print res

