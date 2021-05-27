{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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
  runProgram AnyCompiled path1 [""]
  runProgram AnyCompiled path2 [""]
  runProgram (Python "") path3 [""]

data SomeEffect m a where 
  DoSomething :: SomeEffect m ()

makeSem ''SomeEffect

exampleProgram ::  Member SomeEffect r => String -> Sem r ()
exampleProgram s = doSomething

main :: IO ()
main = do
    h <- openFile "D:/Thesis/error-correcting-code-tester/temp/img.png" ReadMode
    exampleProgram

