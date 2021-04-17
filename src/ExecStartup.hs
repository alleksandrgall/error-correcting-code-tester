{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module ExecStartup where

import Common.Logger
import qualified Data.ByteString as B
import Data.Map
import Data.Text
import Polysemy
import System.Clock (Clock (..), TimeSpec, diffTimeSpec, getTime)
import System.Process
import Prelude hiding (readFile)

--To be defined

data ExecInputType = AsByteString | MaybeToBeDefined

data ExecInput = ByteStringInput B.ByteString | MaybeToBeDefinedInput String

data ExecStartupConfig = ExecStartupConfig
  { execPath :: FilePath,
    params :: [Text],
    inputPath :: FilePath,
    execInputType :: ExecInputType
  }

data ExecResults = ExecResults
  { execTime :: TimeSpec,
    execResult :: String
  }

data ExecLang m a where
  RunExec :: FilePath -> [Text] -> ExecInput -> ExecLang m ExecResults
  ReadFileAsByteString :: FilePath -> ExecLang m ExecInput

makeSem ''ExecLang

executeFile :: Members '[Logger, ExecLang] r => ExecStartupConfig -> Sem r ExecResults
executeFile config = do
  logInfo $ "Reading input for executable " ++ execPath config
  inp <- case execInputType config of
    AsByteString -> do
      readFileAsByteString $ inputPath config
    MaybeToBeDefined -> do
      logError "This is not supposed to happen"
      pure $ MaybeToBeDefinedInput ""
  logInfo $ "Reading is finished for executable " ++ execPath config
  logInfo $ "Running executable " ++ execPath config
  res <- runExec (execPath config) (params config) inp
  logInfo $ "Executable " ++ execPath config ++ "finished successfully"
  return res
