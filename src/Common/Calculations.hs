{-# LANGUAGE BlockArguments #-}
module Common.Calculations where

import Polysemy
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Bits as Bi
import Data.List
import Control.Monad
import Data.Monoid
import System.IO
import Experiment.InbuildNoise
import HexCalc
import Control.DeepSeq
import Control.Exception


getErrorRate :: B.ByteString -> B.ByteString -> Double
getErrorRate orig corrupted = let smallestLength = min (B.length orig) (B.length corrupted)
                                  errorCount = sum $ zipWith (\origW corW -> Bi.popCount (origW `xor` corW)) (B.unpack orig) (B.unpack corrupted)
                              in fromIntegral errorCount / fromIntegral (smallestLength * 8)

getErrorRateWordBool :: Int -> [Bool] -> [Bool] -> Double
getErrorRateWordBool len orig corrupted =
  let wordsInOrig = splitEvery len orig
      wordsInCorrupted = splitEvery len corrupted
  in force $ (fromIntegral . sum . zipWith (\w1 w2 -> fromEnum (w1 /= w2)) wordsInOrig $ wordsInCorrupted) /(fromIntegral . length $ wordsInCorrupted)

getErrorRateBool :: [Bool] -> [Bool] -> Double
getErrorRateBool orig corrupted =  (fromIntegral . sum . zipWith (\b1 b2 -> fromEnum (b1 == not b2)) orig $ corrupted) / (fromIntegral . length $ orig)

data Calculation i m a where
  ErrorInChannelToErrorInEncodedChannelWord :: Bool -> i -> [((i, Int), Double)] ->  Calculation i m [(Double, Double)]
  CountWordErrorRate :: i -> (i, Int) -> Calculation i m Double
  ErrorInChannelToErrorInEncodedChannelBit :: Bool -> i -> [((i, Int), Double)] ->  Calculation i m [(Double, Double)]
--Валидация программы через формулу в вотсапе
--  CompetionToIzbitochnost' :: Calculation m (Double, Double)
--  ErrorInChannelToErrorInEncodedChannelBit :: Calculation m (Double, Double)
--  CompetionToIzbitochnost'Word :: Calculation m (Double, Double)

makeSem ''Calculation


zipWithCompensationb :: [Bool] -> (([Bool], Int), Double) -> (Double, Double)
zipWithCompensationb mes ((dec, k), p) =  (p, 1 - (1 - getErrorRateWordBool k dec mes) ** ((1 :: Double) / fromIntegral k))

zipNoCompensationb :: [Bool] -> (([Bool], Int), Double) -> (Double, Double)
zipNoCompensationb mes ((dec, k), p) = (p, getErrorRateWordBool k dec mes)

interpreterCalcBool :: forall r a . Sem (Calculation [Bool] ': r) a -> Sem r a
interpreterCalcBool = interpret \case
  CountWordErrorRate mes (dec, n) -> do
    let res = getErrorRateWordBool n mes dec
    return $ force res
    
  ErrorInChannelToErrorInEncodedChannelWord compensationForDimFlag mes dec -> do
    let
      res = if compensationForDimFlag then
             map (zipWithCompensationb mes) dec
            else
             map (zipNoCompensationb mes) dec
    return $ force res

interpreterCalc :: forall r a . Member (Embed IO) r => Sem (Calculation FilePath ': r) a -> Sem r a
interpreterCalc = interpret \case
  ErrorInChannelToErrorInEncodedChannelWord compensationForDimFlag mes dec -> do
    hM <- embed $ openFile mes ReadMode
    embed $ hSetBuffering hM (BlockBuffering (Just 64))
    mesBs <- embed $ B.hGetContents hM
    let bools = readBytestringAsBoolList mesBs
    res <- if compensationForDimFlag then
      embed (mapM (zipWithCompensation bools) dec  :: IO [(Double, Double)])
    else
      embed (mapM (zipNoCompensation bools) dec :: IO [(Double, Double)])
    embed $ hClose hM
    embed $ evaluate res
    where
      zipWithCompensation mesBs ((fp, k), p) = do
        h <- openFile fp ReadMode
        hSetBuffering h (BlockBuffering (Just 64))
        bs <- B.hGetContents h
        let
          bools = readBytestringAsBoolList bs
          erRate = 1 - (1 - getErrorRateWordBool k bools mesBs) ** ((1 :: Double) / (fromIntegral k))
        hClose h
        return (p, erRate)
      zipNoCompensation mesBs ((fp, k), p) = do
        h <- openFile fp ReadMode
        hSetBuffering h (BlockBuffering (Just 64))
        bs <- B.hGetContents h
        let
          bools = readBytestringAsBoolList bs 
          erRate = getErrorRateWordBool k bools mesBs
        hClose h
        return (p, erRate)