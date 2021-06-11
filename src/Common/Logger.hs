{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Common.Logger
  ( Logger(..),
    runLoggerToStdOut,
    runLoggerToStdOutUnsafeWithTime,
    runLoggerToFileUnsafeWithTime,
    runLoggerToFileUnsafe,
    runLoggerOutput,
    logInfo,
    logDebug,
    logError,
    logWarning,
  )
where

---Redact---
import Data.Time.Clock (getCurrentTime)
import Polysemy
import Polysemy.Output
import Common.Plot
import Common.ChainProcess
import System.IO

data Logger m a where
  LogInfo :: String -> Logger m ()
  LogError :: String -> Logger m ()
  LogWarning :: String -> Logger m ()
  LogDebug :: String -> Logger m ()

makeSem ''Logger

--addLogging :: forall s r a. Members '[ChainProcess s, ChartLang, Logger] r => Sem r a -> Sem r a
--addLogging = intercept \case
--  MakeChart cp ff -> do
--    fp <- makeChart cp ff
--    logInfo $ "Chart with name " ++ legend cp ++ " to " ++ fp ++ " with plots:\n " ++ foldl (\str p -> str ++ "Name " ++ plotName p ++ " color " ++ (show . color $ p) ++ "\n") "" (plotsDouble cp)
--    return fp
--  SetInput h -> do
--    setInput h
--    logInfo "New input set"
--  RunProgram pInfo -> do
--    runProgram pInfo
--    --logInfo $ "Executable at path " ++ path ++ " with arguments " ++ args ++ " successfully launched"

runLoggerToIOUnsafe :: Member (Embed IO) r => (String -> IO ()) -> Sem (Logger ': r) a -> Sem r a
runLoggerToIOUnsafe out =
  runOutputSem (embed . out) . reinterpret \case
    LogError msg -> output $ "error: " ++ msg ++ "\n"
    LogInfo msg -> output $ "info: " ++ msg ++ "\n"
    LogWarning msg -> output $ "warning: " ++ msg ++ "\n"
    LogDebug msg -> output $ "debug: " ++ msg ++ "\n"

runLoggerToStdOut :: Member (Embed IO) r => Sem (Logger ': r) a -> Sem r a
runLoggerToStdOut = runLoggerToIOUnsafe putStr

runLoggerToFileUnsafe :: Members '[Embed IO] r => FilePath -> Sem (Logger ': r) a -> Sem r a
runLoggerToFileUnsafe fp = runLoggerToIOUnsafe $ appendFile fp

runLoggerOutput :: Sem (Logger ': r) a -> Sem r ([String], a)
runLoggerOutput =
  runOutputMonoid pure . reinterpret \case
    LogInfo msg -> output $ "info: " ++ msg ++ "\n"
    LogError msg -> output $ "error: " ++ msg ++ "\n"
    LogWarning msg -> output $ "warning: " ++ msg ++ "\n"
    LogDebug msg -> output $ "debug: " ++ msg ++ "\n"

runLoggerToIOUnsafeWithTime :: Member (Embed IO) r => (String -> IO ()) -> Sem (Logger ': r) a -> Sem r a
runLoggerToIOUnsafeWithTime out =
  let outWithTime = \s -> getCurrentTime >>= \t -> out $ ("[" ++ show t ++ "]> ") ++ s
   in runLoggerToIOUnsafe outWithTime

runLoggerToFileUnsafeWithTime :: Member (Embed IO) r => FilePath -> Sem (Logger ': r) a -> Sem r a
runLoggerToFileUnsafeWithTime fp = runLoggerToIOUnsafeWithTime $ appendFile fp

runLoggerToStdOutUnsafeWithTime :: Member (Embed IO) r => Sem (Logger ': r) a -> Sem r a
runLoggerToStdOutUnsafeWithTime = runLoggerToIOUnsafeWithTime putStr

