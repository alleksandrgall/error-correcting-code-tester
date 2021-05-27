{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
module Common.ChainProcess where

import Logger
import Polysemy
import Polysemy.State as S
import Polysemy.Output as O
import System.IO
import System.Process as P
import Control.Exception as E
import Control.Monad (void)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

type ProgPath = String

type Args = [String]

data Tree s = Leaf s | Branch (Tree s) (Tree s)

randomTree :: Tree String
randomTree = Branch (Leaf "s") (Leaf "s")
 
data ProgLang
  = AnyCompiled
  | Python
      FilePath --env up .bat file (commonly in venv folder)

data ChainProcess s m a where
  SetInput :: s -> ChainProcess s m ()
  --SetInputPure :: ByteString -> ChainProcess m () -- for tests
  RunProgram :: ProgLang -> ProgPath -> Args -> ChainProcess s m ()

makeSem ''ChainProcess

simpleProgram :: Member (ChainProcess Handle) r => Handle -> Sem r ()
simpleProgram h = do
  setInput h
  runProgram AnyCompiled "" [""]

interpretChainProcessHandle :: Sem (ChainProcess Handle ': r) a -> Sem (State Handle ': Embed IO ': r) a
interpretChainProcessHandle = reinterpret2 \case
  SetInput h -> put h
  RunProgram AnyCompiled path args -> do
    h <- get
    (_, hout, _, _) <- embed $ P.createProcess (proc path args) {
      std_in = UseHandle h
    , std_out = CreatePipe}
    put $ fromJust hout
  RunProgram (Python envUper) path args -> do
    h <- get
    s <- embed $ P.readCreateProcess (shell (envUper ++ " && set")) ""
    (_, hout, _, _) <- embed $ createProcess (shell $ "python " ++ path ++ concat args) {
      env = Just $ rawEnvToList s
    , std_in = UseHandle h
    , std_out = CreatePipe}
    put $ fromJust hout
  where 
    rawEnvToList :: String -> [(String, String)]
    rawEnvToList s =
       let lns = lines s
       in map (\str -> (takeWhile (/= '=') str, tail . dropWhile (/= '=') $ str) ) lns
       
funcExemp :: (Int -> Int) -> [Int] -> [Int]
funcExemp = fmap  
 
interpretChainProcessTest :: Sem (ChainProcess Int ': r) a -> Sem (State Int ': r) ([String], a)
interpretChainProcessTest = O.runOutputList . reinterpret2 \case
  SetInput h -> put h
  RunProgram AnyCompiled path args -> do
    desiredInput <- get
    output @String $ "Run precompiled programm with stdin " ++ show desiredInput ++ " and path: " ++ path ++ "\nargs: " ++ show args
    modify (+ 1)
  RunProgram (Python envUper) path args -> do
    desiredInput <- get
    output @String $ "Run Python script with stdin " ++ show desiredInput ++ "\nenv upper: " ++ envUper ++ "\npath: " ++ path ++ "\nargs: " ++ show args
    modify (+ 1)

addLogging :: Member Logger r => Sem (ChainProcess Handle ': r) a -> Sem (ChainProcess Handle ': r) a
addLogging = intercept \case
  SetInput h -> do
    setInput h
    logInfo "added new input"
  RunProgram AnyCompiled path args -> do
    runProgram AnyCompiled path args
    logInfo "compyled launched"
  RunProgram (Python e) path args -> do
    runProgram (Python e) path args
    logInfo "python launched"

