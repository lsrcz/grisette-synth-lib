{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Grisette.Lib.Synth.Reasoning.Parallel.LogConfig
  ( LogConfig (..),
    logRootDir,
    getLogConfig,
    getDefaultLogger,
  )
where

import qualified Data.Text as T
import Data.Time (getZonedTime)
import System.Directory.Extra (createDirectoryIfMissing)
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (LogHandler (setFormatter))
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (Logger, Priority (DEBUG, NOTICE), getLogger, removeHandler, rootLoggerName, saveGlobalLogger, setHandlers, setLevel, updateGlobalLogger)

data LogConfig = LogConfig
  { baseDir :: FilePath,
    progName :: T.Text,
    timeStamp :: String
  }

logRootDir :: LogConfig -> FilePath
logRootDir LogConfig {..} =
  baseDir <> "/" <> T.unpack progName <> "/" <> timeStamp

getLogConfig :: FilePath -> String -> IO LogConfig
getLogConfig baseDir progName = do
  timeStamp <- fmap (\case ' ' -> '-'; c -> c) . show <$> getZonedTime
  let logConfig = LogConfig {baseDir, progName = T.pack progName, timeStamp}
  createDirectoryIfMissing True (logRootDir logConfig)
  return logConfig

getDefaultLogger :: LogConfig -> Bool -> IO Logger
getDefaultLogger logConfig@LogConfig {..} enableDebugLogging = do
  logger <- getLogger (T.unpack progName)
  updateGlobalLogger rootLoggerName removeHandler
  let defaultFormatter lh =
        return $
          setFormatter
            lh
            (simpleLogFormatter "[$time : $loggername : $prio] $msg")
  let rootDir = logRootDir logConfig
  hdebug <- fileHandler (rootDir <> "/debug.log") DEBUG >>= defaultFormatter
  h <- fileHandler (rootDir <> "/notice.log") NOTICE >>= defaultFormatter
  hstderr <- streamHandler stderr NOTICE >>= defaultFormatter
  let logger' =
        setHandlers ([h, hstderr] ++ [hdebug | enableDebugLogging])
          . setLevel DEBUG
          . removeHandler
          $ logger
  saveGlobalLogger logger'
  return logger'
