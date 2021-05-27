module Main where

import qualified Sandbox
import Polysemy
import Control.Composition ((&))
import Polysemy.State
import System.Process as P
import System.IO
import Control.Monad (void)
import ChainProcess
import Logger

chainProg3 :: Member (ChainProcess Handle) r => ProgPath -> ProgPath -> ProgPath -> Handle ->  Sem r ()
chainProg3 path1 path2 path3 stdin = do
  setInput stdin
  runProgram AnyCompiled path1 ["fuck"]
  runProgram AnyCompiled path2 ["shit"]
  runProgram (Python "activate.bat") path3 ["yo"]

main :: IO ()
main = do
    undefined
--  h <- openFile "D:/Thesis/error-correcting-code-tester/temp/img.png" ReadMode
--  chainProg3
  
