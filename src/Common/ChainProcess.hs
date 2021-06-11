{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Common.ChainProcess where

import Control.Exception as E
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Polysemy
import Polysemy.Output as O
import Polysemy.State as S
import System.IO
import System.Process as P
import Experiment.InbuildNoise
import Experiment.Core
import HexCalc
import Data.List (words)
import qualified Data.Binary as Bin
import Text.Printf (printf)

type ProgPath = String

type Args = [String]

data Tree s = Leaf s | Branch (Tree s) (Tree s)

data ChainProcess s m a where
  InputFile :: FilePath -> ChainProcess s m ()
  OutputFile :: FilePath -> ChainProcess s m ()
  OutputFileTemp :: ChainProcess s m FilePath
  SetInput :: s -> ChainProcess s m ()
  RunProgram :: ProgramInfo -> ChainProcess s m ()

makeSem ''ChainProcess

wordByWord :: FilePath -> Int -> [Bool] -> IO [Bool]
wordByWord fp par mes = do
  let hexMes = boolsToHexList par mes
  res <- whileMonoidM (not . null) (\(l:listOfWords) -> do
    (_, Just hout, _, _) <- P.createProcess (proc fp [l, show par]) { std_out = CreatePipe }
    str <- hGetContents hout
    hClose hout
    return (listOfWords, [head . words $ str])
    ) hexMes
  return $ hexListToBools . reverse $ res

parseArgs :: String -> (Int, Int)
parseArgs args =
  let nk = words args
  in (read . head $ nk, read . last $ nk)

interpretChainProcessHandleHamming :: forall r a . FilePath -> Sem (ChainProcess [Bool] ': r) a -> Sem (State ([Bool], String) ': Embed IO ': r) a
interpretChainProcessHandleHamming dir = reinterpret2 \case
  InputFile fp -> do
     bools <- embed $ withFile fp ReadMode
        (\h -> readBytestringAsBoolList <$> (hSetBuffering h (BlockBuffering (Just 64)) >> B.hGetContents h))
     put (bools, "")

  OutputFile fp -> do
    (boolList, _) <- get
    embed $ B.writeFile fp (boolListToBytestring boolList)

  OutputFileTemp -> do
    (boolList, fname) <- get
    (newPath, h) <- embed $ openTempFile dir fname
    embed $ hClose h
    embed $ B.writeFile newPath (boolListToBytestring boolList)
    return newPath
--
  SetInput inp -> put (inp, "")
--
  RunProgram (Inbuild (Even p)) -> do
    (boolList, curName) <- get
    --(newPath, h) <- embed $ openTempFile dir $ "even_errors_" ++ printf ("%0." ++ show 6 ++ "f") p
    --embed $ hClose h
    corrupted <- embed $ evenErrorsBool boolList p
    --embed $ B.writeFile newPath (boolListToBytestring corrupted)
    put (corrupted,  if null curName then "Noiseeven_errors_" ++ printf ("%0." ++ show (6 :: Int) ++ "f") p else
        curName ++ "&&even_errors_" ++ printf ("%0." ++ show (6 :: Int) ++ "f") p)
--
  RunProgram (Inbuild (Burst p (b1, b2))) -> do
    (boolList, curName) <- get
    corrupted <- embed (burstErrorsBool (b1, b2) boolList p)
    put (corrupted, if null curName then "Noiseburst_errors_" ++ show b1 ++ show b2 ++ printf ("%0." ++ show (6 :: Int) ++ "f") p else
        curName ++ "&&burst_errors_" ++ show b1 ++ show b2 ++ printf ("%0." ++ show (6 :: Int) ++ "f") p )
--
  RunProgram (ProgramInfo pType pPath args maybeEnv) -> do
    (bools, curName) <- get
    let (n, k) = parseArgs args
    newBools <- embed $ wordByWord pPath (if pType == Coder then k else n) bools
    put (newBools, if null curName then show pType ++ pPath ++ args else curName ++ "&&" ++ pPath ++ args)
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


