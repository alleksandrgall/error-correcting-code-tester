module Experiment.InbuildInterleaver where

import HexCalc
import Data.List
import Experiment.InbuildNoise
import Control.DeepSeq

blockInterleaver :: Int -> Int -> [Bool] -> ([Bool], Int)
blockInterleaver n m bools =
  let (bools', leadingZeroes) = addLeadingZeroes (n*m) bools
      interleaved = concatMap (\word -> force . concat . transpose $ (splitEvery n word)) (splitEvery (n*m) bools')
  in  (force interleaved, leadingZeroes)

blockUnInterleaver :: Int -> Int -> Int -> [Bool] -> [Bool]
blockUnInterleaver n m leadingZeroes bools =
  let uninterleaved = concatMap (\word -> force . concat . transpose $ (splitEvery m word)) (splitEvery (n*m) bools)
  in force $ drop leadingZeroes uninterleaved

