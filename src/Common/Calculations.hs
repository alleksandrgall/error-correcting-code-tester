{-# LANGUAGE BlockArguments #-}
module Common.Calculations where

import Polysemy
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Bits as Bi
import Data.List
import Control.Monad
import System.IO



getErrorRate :: B.ByteString -> B.ByteString -> Double
getErrorRate orig corrupted = let smallestLength = min (B.length orig) (B.length corrupted)
                                  errorCount = sum $ zipWith (\origW corW -> Bi.popCount (origW `xor` corW)) (B.unpack orig) (B.unpack corrupted)
                              in fromIntegral errorCount / fromIntegral (smallestLength * 8)

getErrorRateBool :: [Bool] -> [Bool] -> Double
getErrorRateBool orig corrupted =  (fromIntegral . sum . zipWith (\b1 b2 -> fromEnum (b1 == not b2)) orig $ corrupted) / (fromIntegral . length $ orig)

data InputType a = InputType

data Calculation m a where
  ErrorInChannelToErrorInEncodedChannelWord :: Bool -> FilePath -> [(FilePath, Int, Int)] -> [Double] ->  Calculation m [(Double, Double)]
--Валидация программы через формулу в вотсапе
--  CompetionToIzbitochnost' :: Calculation m (Double, Double)
--  ErrorInChannelToErrorInEncodedChannelBit :: Calculation m (Double, Double)
--  CompetionToIzbitochnost'Word :: Calculation m (Double, Double)

makeSem ''Calculation

interpreterCalc :: Member (Embed IO) r => Sem (Calculation ': r) a -> Sem r a
interpreterCalc = interpret \case
  ErrorInChannelToErrorInEncodedChannelWord compensationForDimFlag mes decodeds probErrs -> do
    hM <- embed $ openFile mes ReadMode
    embed $ hSetBuffering hM (BlockBuffering (Just 64))
    mesBs <- embed $ B.hGetContents hM
    res <- if compensationForDimFlag then
      embed (zipWithM (zipWithCompensation mesBs) decodeds probErrs :: IO [(Double, Double)])
    else
      embed (zipWithM (zipNoCompensation mesBs) decodeds probErrs :: IO [(Double, Double)])
    embed $ hClose hM
    return res
    where
      zipWithCompensation mesBs (fp, n, k) p = do
        h <- openFile fp ReadMode
        hSetBuffering h (BlockBuffering (Just 64))
        bs <- B.hGetContents h
        let erRate = 1 - (1 - getErrorRate bs mesBs) ** ((1 :: Double) / (fromIntegral k))
        hClose h
        return (erRate, p)
      zipNoCompensation mesBs (fp, n, k) p = do
        h <- openFile fp ReadMode
        hSetBuffering h (BlockBuffering (Just 64))
        bs <- B.hGetContents h
        let erRate = getErrorRate bs mesBs
        hClose h
        return (erRate, p)