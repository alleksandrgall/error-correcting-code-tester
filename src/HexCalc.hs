module HexCalc where

import qualified Data.ByteString as B
import Data.Bits
import Text.Printf (printf)
import Data.Binary
import Experiment.InbuildNoise
import Data.List (foldl')
import Control.Monad
import Numeric
import Control.Parallel.Strategies

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM test act st =
   when (test st) $ (act st) >>= whileM test act

whileMonoidM :: (Monad m, Monoid b) => (a -> Bool) -> (a -> m (a, b)) -> a -> m b
whileMonoidM test act st =
   if test st then act st >>= \(newSt, newAcc) -> liftM2 (<>) (return newAcc) (whileMonoidM test act newSt) else pure mempty

boolsToHexList :: Int -> [Bool] -> [String]
boolsToHexList wordLength bools = map (\x -> "0x" ++ (printf "%x" . snd . foldl' (\(pow, acc) b ->
  (pow - 1, (fromEnum b) * 2^pow + acc)) (wordLength - 1, 0) $ x))  (splitEvery wordLength bools) `using` parList rdeepseq

boolsToHexListNoComp :: Int -> [Bool] -> [String]
boolsToHexListNoComp wordLength bools = map (\x -> "0x" ++ (printf "%x" . snd . foldl' (\(pow, acc) b ->
  (pow - 1, (fromEnum b) * 2^pow + acc)) (wordLength - 1, 0) $ x))  (splitEvery wordLength bools) 
  --
--
hexListToBools :: Int -> [String] -> [Bool]
hexListToBools l str = 
  let res =  map (\hex ->  let (int :: Int) = fst. head . readHex . tail . dropWhile (/='x') $ hex in stringToBool . 
        printf ("%" ++ "0" ++ (show l) ++ "b") $ int) str `using` parList rdeepseq
  in concat res   
  where stringToBool :: String -> [Bool]
        stringToBool = reverse . foldl' (\acc c -> (c == '1'):acc) []
        
hexListToBoolsNoComp :: [String] -> [Bool]
hexListToBoolsNoComp str = 
  let res =  map (\hex ->  let (int :: Int) = fst. head . readHex . tail . dropWhile (/='x') $ hex in stringToBool . printf ("%" ++ "0b") $ int) str `using` parList rdeepseq
  in concat res   
  where stringToBool :: String -> [Bool]
        stringToBool = reverse . foldl' (\acc c -> (c == '1'):acc) []

readBytestringAsBoolList :: B.ByteString -> [Bool]
readBytestringAsBoolList bs = concatMap (\x -> [testBit x i | i <- [0.. finiteBitSize x - 1]]) (B.unpack bs)

boolListToBytestring :: [Bool] -> B.ByteString
boolListToBytestring bools = B.pack . map snd $ [ foldl' (\(i, word) bit -> if bit then (i + 1, setBit word i) else (i + 1, word)) (0, 0 :: Word8) x | x <- splitEvery 8 bools ]

