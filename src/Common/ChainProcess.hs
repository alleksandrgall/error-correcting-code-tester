{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Common.ChainProcess where

import qualified Data.ByteString as B
import Polysemy
import Polysemy.State as S
import System.IO
import System.Process as P
import Experiment.InbuildNoise
import Experiment.InbuildInterleaver
import Experiment.Core
import HexCalc
import Text.Printf (printf)
import Control.DeepSeq (force)
import Control.Exception (evaluate)

type ProgPath = String

type Args = [String]

type Inp s = (s, [Int])

inpMes :: (s, [Int]) -> s
inpMes = fst

leadingZeroes :: (s, [Int]) -> [Int]
leadingZeroes = snd

data ChainProcess s m a where
  InputFile :: FilePath -> ChainProcess s m s
  OutputFile ::  FilePath -> ChainProcess s m ()
  OutputFileTemp :: ChainProcess s m FilePath
  SetInput :: s -> ChainProcess s m ()
  GetInput :: ChainProcess s m s
  RunProgram :: ProgramInfo -> ChainProcess s m ()

makeSem ''ChainProcess

parseArgs :: String -> (Int, Int)
parseArgs args =
  let nk = words args
  in (read . head $ nk, read . last $ nk)

interpretChainProcessHandle :: forall r a . Member (Embed IO) r => FilePath -> Sem (ChainProcess (Inp [Bool]) ': r) a -> Sem (State (Inp [Bool], String) ': r) a
interpretChainProcessHandle dir = reinterpret \case
  InputFile fp -> do
     bools <- embed $ withFile fp ReadMode (\h -> readBytestringAsBoolList <$> (hSetBuffering h (BlockBuffering (Just 64)) >> B.hGetContents h))
     put ((bools, []), "")
     return $ force (bools <> bools <> bools <> bools, [])
     
  GetInput -> do
     ((bools, ints), _) <- get
     return (bools, ints)

  OutputFile fp -> do
    ((boolList, ints), _) <- get
    embed $ B.writeFile fp (boolListToBytestring boolList)
    put ((boolList, ints), "")

  OutputFileTemp -> do
    ((boolList, ints), fname) <- get
    (newPath, h) <- embed $ openTempFile dir fname
    embed $ hClose h
    embed $ B.writeFile newPath (boolListToBytestring boolList)
    put ((boolList, ints), "")
    return newPath
--
  SetInput inp ->
    put (inp, "")
--
  RunProgram (Inbuild (Even p)) -> do
    ((boolList, ints), curName) <- get
    corrupted <- embed $ evenErrorsBool boolList p
    put ((corrupted, ints),  if null curName then "Noise_even_errors_" ++ printf ("%0." ++ show (6 :: Int) ++ "f") p else
        curName ++ "&&even_errors_" ++ printf ("%0." ++ show (6 :: Int) ++ "f") p)
--
  RunProgram (Inbuild (Burst p (b1, b2))) -> do
    ((boolList, ints), curName) <- get
    corrupted <- embed (burstErrorsBool (b1, b2) boolList p)
    put ((corrupted, ints), if null curName then "Noise_burst_errors_" ++ show b1 ++ show b2 ++ printf ("%0." ++ show (6 :: Int) ++ "f") p else
        curName ++ "&&burst_errors_" ++ show b1 ++ show b2 ++ printf ("%0." ++ show (6 :: Int) ++ "f") p )
  RunProgram (Inbuild (Guilbert p q pp)) -> do
    ((boolList, ints), curName) <- get
    corrupted <- embed $ guilbertBool boolList p q pp
    put ((corrupted, ints), if null curName then "Noise_Guilbert_" ++ show p ++ "_" ++ show q ++ "_" ++ show pp else
      curName ++ "&&_Guilbert_" ++ show p ++ "_" ++ show q ++ "_" ++ show pp) 
--
  RunProgram (Inbuild (Interleaver n m )) -> do
    ((boolList, ints), curName) <- get
    (interleaved, int) <- embed $ evaluate .force . blockInterleaver n m $ boolList
    put ((interleaved, int:ints), (if null curName then "" else "&&") ++ "Block_interleaver_"++show n ++ "_" ++ show m )

  RunProgram (Inbuild (UnInterleaver n m)) -> do
     ((boolList, ints), curName) <- get
     uninterleaved <- embed $ evaluate . force .  blockUnInterleaver n m (head ints) $ boolList
     put ((uninterleaved, tail ints), (if null curName then "" else "&&") ++ "Block_uninterleaver_"++show n ++ "_" ++ show m )

  RunProgram pi@(ProgramInfo pType pPath args maybeEnv) -> do
    if notExists pPath then
        haulting pi
    else do
      let (n, k) = parseArgs args
      (inp, curName) <- get
      newInp <- embed $ wordByWord pPath (if pType == Coder then (k, n) else (n, k)) inp
      put (newInp, if null curName then show pType ++ pPath ++ args else curName ++ "&&" ++ pPath ++ args)


--    if pPath == "D:/hamming-codec/build/bin/example_encode.exe" then do
--      (newBools, newInt) <- embed $ evaluate $ force (hamming74Encode bools)
--      put ((newBools, newInt:ints), if null curName then show pType ++ pPath ++ args else curName ++ "&&" ++ pPath ++ args)
--    else
--      if pPath == "D:/hamming-codec/build/bin/example_decode.exe" then do
--        newBools <- embed $ evaluate $ force (hamming74Decode (head ints) bools)
--        put ((newBools, tail ints), if null curName then show pType ++ pPath ++ args else curName ++ "&&" ++ pPath ++ args)
--      else do
--        undefined






wordByWord :: FilePath -> (Int, Int) -> Inp [Bool] -> IO (Inp [Bool])
wordByWord fp (park, parn) (mes, ints) = do
  let hexMes = force $ boolsToHexList park mes
  res <- whileMonoidM (not . null) (\(l:listOfWords) -> do
    (_, Just hout, _, _) <- P.createProcess (proc fp [l, show park]) { std_out = CreatePipe }
    str <- hGetContents hout
    res <- evaluate $ force (listOfWords, [head . words $ str])
    hClose hout
    return res
    ) hexMes
  newMes <- evaluate . force . hexListToBools parn . reverse $ res
  return (newMes, ints)

















--interpretChainProcessHandle :: Sem (ChainProcess Handle ': r) a -> Sem (State Handle ': Embed IO ': r) a
--interpretChainProcessHandle = reinterpret2 \case
--  InputFile fp -> embed . openFile fp $ ReadMode >>= put
--  OutputFile fp -> get >>= embed . B.hGetContents >>= B.writeFile fp
--  SetInput h -> put h
--  RunProgram AnyCompiled path args -> do
--    h <- get
--    (_, hout, _, _) <-
--      embed $
--        P.createProcess
--          (proc path args)
--            { std_in = UseHandle h,
--              std_out = CreatePipe
--            }
--    put $ fromJust hout
--  RunProgram (Python envUper) path args -> do
--    h <- get
--    s <- embed $ P.readCreateProcess (shell (envUper ++ " && set")) ""
--    (_, hout, _, _) <-
--      embed $
--        createProcess
--          (shell $ "python " ++ path ++ concat args)
--            { env = Just $ rawEnvToList s,
--              std_in = UseHandle h,
--              std_out = CreatePipe
--            }
--    put $ fromJust hout
--  where
--    rawEnvToList :: String -> [(String, String)]
--    rawEnvToList s =
--      let lns = lines s
--       in map (\str -> (takeWhile (/= '=') str, tail . dropWhile (/= '=') $ str)) lns
--
--funcExemp :: (Int -> Int) -> [Int] -> [Int]
--funcExemp = fmap
--
--interpretChainProcessTest :: Sem (ChainProcess Int ': r) a -> Sem (State Int ': r) ([String], a)
--interpretChainProcessTest =
--  O.runOutputList . reinterpret2 \case
--    SetInput h -> put h
--    RunProgram AnyCompiled path args -> do
--      desiredInput <- get
--      output @String $ "Run precompiled programm with stdin " ++ show desiredInput ++ " and path: " ++ path ++ "\nargs: " ++ show args
--      modify (+ 1)
--    RunProgram (Python envUper) path args -> do
--      desiredInput <- get
--      output @String $ "Run Python script with stdin " ++ show desiredInput ++ "\nenv upper: " ++ envUper ++ "\npath: " ++ path ++ "\nargs: " ++ show args
--      modify (+ 1)

notExists :: FilePath -> Bool
notExists fp= fp == "D:/hamming-codec/build/bin/example_encode.exe" || fp == "D:/hamming-codec/build/bin/example_decode.exe"

haulting :: forall r . Member (Embed IO) r => ProgramInfo -> Sem (State (Inp [Bool], String) ': r) ()
haulting (ProgramInfo pType pPath args maybeEnv) = do
  let (n, k) = parseArgs args
  ((bools, ints), curName) <- get
  if pPath == "D:/hamming-codec/build/bin/example_encode.exe" && k == 4 then do
    (newBools, newInt) <- embed $ evaluate $ force (hamming74Encode bools)
    put ((newBools, newInt:ints), if null curName then show pType ++ pPath ++ args else curName ++ "&&" ++ pPath ++ args)
  else
    if pPath == "D:/hamming-codec/build/bin/example_decode.exe" && n == 7 then do
      newBools <- embed $ evaluate $ force (hamming74Decode (head ints) bools)
      put ((newBools, tail ints), if null curName then show pType ++ pPath ++ args else curName ++ "&&" ++ pPath ++ args)
    else do
      if pPath == "D:/hamming-codec/build/bin/example_encode.exe" && k == 11 then do
         (newBools, newInt) <- embed $ evaluate $ force (hamming1511Encode bools)
         put ((newBools, newInt:ints), if null curName then show pType ++ pPath ++ args else curName ++ "&&" ++ pPath ++ args)
         else do
           newBools <- embed $ evaluate $ force (hamming1511Decode (head ints) bools)
           put ((newBools, tail ints), if null curName then show pType ++ pPath ++ args else curName ++ "&&" ++ pPath ++ args)

