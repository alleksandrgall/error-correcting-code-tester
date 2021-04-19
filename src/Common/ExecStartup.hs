{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.ExecStartup
  ( ExecInputType (..),
    ExecStartupConfig (..),
    executeFile,
    runExecLang,
    ExecResults (..)
  )
where

import Logger
import qualified Data.ByteString.Char8 as B
import Polysemy
import System.Clock (Clock (..), TimeSpec, diffTimeSpec, getTime)
import System.Exit (ExitCode)
import System.Process.ByteString as SP
import Prelude hiding (readFile)

--To be defined

data ExecInputType = AsByteString | Non | MaybeToBeDefined

data ExecStartupConfig = ExecStartupConfig
  { execPath :: FilePath,
    params :: [String],
    inputPath :: FilePath,
    execInputType :: ExecInputType
  }

data ExecResults = ExecResults
  { execTime :: TimeSpec,
    execResult :: (ExitCode, String, String)
  }

data ExecLang m a where
  RunExec :: FilePath -> [String] -> B.ByteString -> ExecLang m ExecResults
  ReadFileAsByteString :: FilePath -> ExecLang m B.ByteString

makeSem ''ExecLang

executeFile :: Members '[Logger, ExecLang] r => ExecStartupConfig -> Sem r ExecResults
executeFile config = do
  logInfo $ "Reading input for executable " ++ execPath config
  inp <- case execInputType config of
    AsByteString -> readFileAsByteString $ inputPath config
    Non -> return ""
    MaybeToBeDefined -> logError "This is not supposed to happen" >> return ""
  logInfo $ "Reading is finished for executable " ++ execPath config
  logInfo $ "Running executable " ++ execPath config
  res <- runExec (execPath config) (params config) inp
  logInfo $ "Executable " ++ execPath config ++ "finished successfully"
  return res

runExecLang :: Member (Embed IO) r => Sem (ExecLang ': r) a -> Sem r a
runExecLang = interpret \case
  RunExec fp prms inp -> do
    start <- embed $ getTime Monotonic
    (exitCode, bstr1, bstr2) <- embed $ readProcessWithExitCode fp prms inp
    end <- embed $ getTime Monotonic
    return $ ExecResults (diffTimeSpec start end) (exitCode, B.unpack bstr1, B.unpack bstr2)
  ReadFileAsByteString fp -> embed $ B.readFile fp
