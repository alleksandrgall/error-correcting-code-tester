module HexCalc where

import Control.Monad
import System.Process as P
import Control.Parallel.Strategies
import Data.Binary
import Data.Bits
import Data.Bool
import qualified Data.ByteString as B
import Data.List (foldl', transpose)
import Experiment.InbuildNoise
import Numeric
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import System.IO
import Text.Printf (printf)
import Control.DeepSeq

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM test act st =
  when (test st) $ act st >>= whileM test act

whileMonoidM :: (Monad m, Monoid b, NFData b) => (a -> Bool) -> (a -> m (a, b)) -> a -> m b
whileMonoidM test act st =
  if test st then act st >>= \(newSt, newAcc) -> liftM2 (\a b -> force (a <> b)) (return newAcc) (whileMonoidM test act newSt) else pure mempty

boolsToHexList :: Int -> [Bool] -> [String]
boolsToHexList wordLength bools =
  map
    ( \x ->
        "0x"
          ++ ( printf "%x" . snd
                 . foldl'
                   ( \(pow, acc) b ->
                       (pow - 1, fromEnum b * 2 ^ pow + acc)
                   )
                   (wordLength - 1, 0)
                 $ x
             )
    )
    (splitEvery wordLength bools)
    `using` parList rdeepseq

boolsToHexListNoComp :: Int -> [Bool] -> [String]
boolsToHexListNoComp wordLength bools =
  map
    ( \x ->
        "0x"
          ++ ( printf "%x" . snd
                 . foldl'
                   ( \(pow, acc) b ->
                       (pow - 1, fromEnum b * 2 ^ pow + acc)
                   )
                   (wordLength - 1, 0)
                 $ x
             )
    )
    (splitEvery wordLength bools)

--
--
hexListToBools :: Int -> [String] -> [Bool]
hexListToBools l str =
  let res =
        map
          ( \hex ->
              let (int :: Int) = fst . head . readHex . tail . dropWhile (/= 'x') $ hex
               in stringToBool
                    . printf ("%" ++ "0" ++ show l ++ "b")
                    $ int
          )
          str
          `using` parList rdeepseq
   in concat res
  where
    stringToBool :: String -> [Bool]
    stringToBool = reverse . foldl' (\acc c -> (c == '1') : acc) []

hexListToBoolsNoComp :: [String] -> [Bool]
hexListToBoolsNoComp str =
  let res = map (\hex -> let (int :: Int) = fst . head . readHex . tail . dropWhile (/= 'x') $ hex in stringToBool . printf ("%" ++ "0b") $ int) str `using` parList rdeepseq
   in concat res
  where
    stringToBool :: String -> [Bool]
    stringToBool = reverse . foldl' (\acc c -> (c == '1') : acc) []


readBytestringAsBoolList :: B.ByteString -> [Bool]
readBytestringAsBoolList bs = concatMap (\x -> [testBit x i | i <- [0 .. finiteBitSize x - 1]]) (B.unpack bs)

boolListToBytestring :: [Bool] -> B.ByteString
boolListToBytestring bools = B.pack . map snd $ [foldl' (\(i, word) bit -> if bit then (i + 1, setBit word i) else (i + 1, word)) (0, 0 :: Word8) x | x <- splitEvery 8 bools]

addLeadingZeroes :: Int -> [Bool] -> ([Bool], Int)
addLeadingZeroes k mes =
  let lengthModK = length mes `mod` k
      leadingZeros = if lengthModK == 0 then 0 else k - lengthModK
  in (replicate leadingZeros False <> mes, leadingZeros)




























hamming74Encode :: [Bool] -> ([Bool], Int)
hamming74Encode bools =
  let lengthMod4 = length bools `mod` 4
      leadingZeros = if lengthMod4 == 0 then 0 else 4 - lengthMod4
      bool' = replicate leadingZeros False <> bools
      encoded = concatMap (\[b0, b1, b2, b3] -> [b0 `xor` b1 `xor` b3, b0 `xor` b2 `xor` b3, b0, b1 `xor` b2 `xor` b3, b1, b2, b3]) (splitEvery 4 bool')
   in (encoded, leadingZeros)

{-# INLINABLE hamming74Encode #-}

hamming74Decode :: Int -> [Bool] -> [Bool]
hamming74Decode leadingZeros bools =
  let sind :: [Bool] -> Int
      sind =
        ( \word ->
            bool 0 1 (snd . foldl' (\(f, acc) b -> if f then (not f, b `xor` acc) else (not f, acc)) (True, False) $ word)
              + bool 0 2 (snd . foldl' (\(f, acc) b -> if f > (-1) && f < 2 then (f + 1, b `xor` acc) else (if f == 2 then (-1) else f + 1, acc)) (0, False) $ tail word)
              + bool 0 4 (foldl1 xor (drop 3 word))
        )
   in drop leadingZeros . concatMap (\word -> let s = sind word in [(3 == s) `xor` word !! 2, (5 == s) `xor` word !! 4, (6 == s) `xor` word !! 5, (7 == s) `xor` word !! 6]) $ splitEvery 7 bools

{-# INLINABLE hamming74Decode #-}

hamming1511Encode :: [Bool] -> ([Bool], Int)
hamming1511Encode bools =
  let lengthMod11 = length bools `mod` 11
      leadingZeros = if lengthMod11 == 0 then 0 else 11 - lengthMod11
      bool' = replicate leadingZeros False <> bools
      encoded =
        concatMap
          ( \word@[b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10] ->
              [ b0 `xor` b1 `xor` b3 `xor` b4 `xor` b6 `xor` b8 `xor` b10,
                b0 `xor` b2 `xor` b3 `xor` b5 `xor` b6 `xor` b9 `xor` b10,
                b0,
                b1 `xor` b2 `xor` b3 `xor` b7 `xor` b8 `xor` b9 `xor` b10,
                b1,
                b2,
                b3,
                foldl1 xor (drop 4 word) `xor` foldl1 xor (drop 7 word),
                b4,
                b5,
                b6,
                b7,
                b8,
                b9,
                b10
              ]
          )
          (splitEvery 11 bool')
   in (encoded, leadingZeros)

{-# INLINABLE hamming1511Encode #-}

hamming1511Decode :: Int -> [Bool] -> [Bool]
hamming1511Decode leadingZeros bools =
  let sind :: [Bool] -> Int
      sind =
        ( \word@[_, _, _, p3, b1, b2, b3, _, _, _, _, b7, b8, b9, b10] ->
            bool 0 1 (snd . foldl' (\(f, acc) b -> if f then (not f, b `xor` acc) else (not f, acc)) (True, False) $ word)
              + bool 0 2 (snd . foldl' (\(f, acc) b -> if f > (-1) && f < 2 then (f + 1, b `xor` acc) else (if f == 2 then (-1) else f + 1, acc)) (0, False) $ tail word)
              + bool 0 4 (p3 `xor` b1 `xor` b2 `xor` b3 `xor` b7 `xor` b8 `xor` b9 `xor` b10)
              + bool 0 8 (foldl1 xor (drop 7 word))
        )
   in drop leadingZeros
        . concatMap
          ( \word ->
              let s = sind word
               in [ (3 == s) `xor` word !! 2,
                    (5 == s) `xor` word !! 4,
                    (6 == s) `xor` word !! 5,
                    (7 == s) `xor` word !! 6,
                    (9 == s) `xor` word !! 8,
                    (10 == s) `xor` word !! 9,
                    (11 == s) `xor` word !! 10,
                    (12 == s) `xor` word !! 11,
                    (13 == s) `xor` word !! 12,
                    (14 == s) `xor` word !! 13,
                    (15 == s) `xor` word !! 14
                  ]
          )
        $ splitEvery 15 bools
        
{-# INLINABLE hamming1511Decode #-}