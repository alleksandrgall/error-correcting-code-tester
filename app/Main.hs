{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Polysemy
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Exception
import Data.Bits as Bits
import Common.Plot
import ChainProcess
import Logger
import Data.Binary
import Common.DB
import Data.HexString
import Data.GHex
import Experiment.Core
import System.Random
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Experiment.InbuildNoise
import Data.Bits
import Data.Time.Clock as C
import Control.Monad
import Data.Word
import Data.List (foldl')
import Text.Printf (printf)
import Common.Calculations

--connectionString = "DB/test.db"
--
--whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
--whileM test act st =
--   when (test st) $ (act st) >>= whileM test act
--
--channel :: Members '[ChainProcess Handle, Logger, ChartLang] r => ProgramInfo -> ProgramInfo -> ProgramInfo -> (Double, Double) -> Sem r ()
--channel = do
--  undefined
--
--boolsToHexList :: Int -> [Bool] -> [String]
--boolsToHexList wordLength bools = map (\x -> "0x" ++ (printf "%x" . snd . foldl' (\(pow, acc) b ->
--  (pow - 1, (fromEnum b) * 2^pow + acc)) (wordLength - 1, 0) $ x))  (splitEvery wordLength bools)
--
readBytestringAsBoolList :: B.ByteString -> [Bool]
readBytestringAsBoolList bs = concatMap (\x -> [testBit x i | i <- [0.. finiteBitSize x - 1]]) (B.unpack bs)

boolListToBytestring :: [Bool] -> B.ByteString
boolListToBytestring bools = B.pack . map snd $ [ foldl' (\(i, word) bit -> if bit then (i + 1, setBit word i) else (i + 1, word)) (0, 0 :: Word8) x | x <- splitEvery 8 bools ]

path' = "D:/Thesis/error-correcting-code-tester/DB/Files/img.jpg"
path'' = "D:/Thesis/error-correcting-code-tester/DB/Files/img1.jpg"
main :: IO ()
main = do
  b <- withFile path' ReadMode
        (\h -> readBytestringAsBoolList <$> (hSetBuffering h (BlockBuffering (Just 64)) >> B.hGetContents h))
  (newPath, h) <- openTempFile "D:/Thesis/error-correcting-code-tester/temp" "img.png"
  hClose h
  B.writeFile newPath (boolListToBytestring b)
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

