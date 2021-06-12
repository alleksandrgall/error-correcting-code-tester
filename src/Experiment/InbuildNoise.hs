module Experiment.InbuildNoise where

import Data.Bits as B
import Data.ByteString.Lazy (ByteString, pack, unpack, length)
import System.Random
import Data.List
import Data.Bits.ByteString
import Data.Bits.Pdep
import Data.Word
import Control.Exception (evaluate)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first, rest) = splitAt n list

evenErrors :: ByteString -> Double -> IO ByteString
evenErrors mes p = do
  g <- getStdGen
  let (p' :: Float) = if p >= 1 then 1 else realToFrac p
      r =  splitEvery 8 $ randomRs (0 :: Float, 1 :: Float) g
      mes' = pack $ zipWith (randomError 0) r (unpack mes)
      randomError i (p1 : prob) word = if p1 < p'
        then randomError (i + 1) prob (B.complementBit word i)
        else randomError (i + 1) prob word
      randomError _ [] word = word
  return mes'

evenErrorsBool :: [Bool] -> Double -> IO [Bool]
evenErrorsBool mes p = do
  g <- getStdGen
  let (p' :: Float) = realToFrac p
      rs = randomRs (0 :: Float, 1 :: Float) g
  evaluate $ zipWith (\b r -> b /= (r < p')) mes rs

burstErrorsBool :: (Int, Int) -> [Bool] -> Double -> IO [Bool]
burstErrorsBool (lowerBound, higherBound) mes p = do
  g <- getStdGen
  let (p' :: Float) = realToFrac p
      rs = randomRs (0 :: Float, 1 :: Float) g
      (avrgErrorBurstLength :: Float) = fromIntegral (sum [lowerBound .. higherBound]) / fromIntegral (higherBound - lowerBound + 1)
      (burstRate :: Float) = 1 / (avrgErrorBurstLength * ((1 :: Float)/p' - 1) + 1)
  evaluate $ fst . foldl' (\(acc, (i, gen)) (r, b) -> if i > 0 then (not b:acc, (i - 1, gen)) else 
    if r > p' then (b : acc, (i, gen)) else 
      let (newI ,newGen) = randomR (lowerBound, higherBound) gen in ( not b : acc, (newI - 1, newGen))) ([], (0, g)) $ (zip rs mes)

burstErrors :: (Int, Int) -> ByteString -> Double -> IO ByteString
burstErrors (lowerBound, higherBound) mes p = do
  g <- getStdGen
  let (p' :: Float) = if p >= 1 then 1 else realToFrac p
      r = map (zip [0..7]) . splitEvery 8 . randomRs (0 :: Float, 1 :: Float) $ g
      (avrgErrorBurstLength :: Float) = fromIntegral (sum [lowerBound .. higherBound]) / fromIntegral (higherBound - lowerBound + 1)
      (burstRate :: Float) = 1 / (avrgErrorBurstLength * ((1 :: Float)/p' - 1) + 1)
      complementKBitsFromNs 0 0 word8 = word8
      complementKBitsFromNs _ 8 word8 = word8
      complementKBitsFromNs 8 0 word8 = B.complement word8
      complementKBitsFromNs k n word8 = if k <= 0 then word8 else complementKBitsFromNs (k - 1) (n + 1) (B.complementBit word8 n)
      iter :: Bits a => (StdGen, Int, [[(Int, Float)]], [a]) -> a -> (StdGen, Int, [[(Int, Float)]], [a])
      iter (g', 0, r':rs, res) word = let listOfLower = filter ((>) burstRate.snd) r'
                                      in if null listOfLower
                                         then (g', 0, rs, word:res)
                                         else let burstStart = fst . head $ listOfLower
                                                  (lengthOfBurst, newGen) = randomR (lowerBound, higherBound) g'
                                              in (newGen, lengthOfBurst - (8 - burstStart), rs, complementKBitsFromNs lengthOfBurst burstStart word:res)
      iter (g', bitsToComplement, r':rs, res) word =
                  if bitsToComplement < 8
                  then let word' = complementKBitsFromNs bitsToComplement 0 word
                           listOfLower = filter ((>) burstRate.snd) $ drop bitsToComplement r'
                       in if null listOfLower
                          then (g', 0, rs, word':res)
                          else let burstStart = fst . head $ listOfLower
                                   (lengthOfBurst, newGen) = randomR (lowerBound, higherBound) g'
                               in (newGen, lengthOfBurst - (8 - burstStart), rs, complementKBitsFromNs lengthOfBurst burstStart word':res)
                  else (g', bitsToComplement - 8, r':rs, complementKBitsFromNs 8 0 word:res)
      (_, _, _, res) = foldl' iter (g, 0, r, []) (unpack mes)
  return $ pack . reverse $ res