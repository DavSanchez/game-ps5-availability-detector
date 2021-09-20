{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Colog
  ( LogAction,
    Message,
    MessageField (MessageField),
    RichMessage,
    cmapM,
    defaultFieldMap,
    fmtRichMessageDefault,
    logText,
    logTextStderr,
    logTextStdout,
    upgradeMessageAction,
    usingLoggerT,
    withLogTextFile,
  )
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (forever, void)
import Data.Text (Text)
import Data.TypeRepMap (TypeRepMap, delete)
import PS5 (ps5check)
import System.Environment (getEnv)

main :: IO ()
main = forever ps5

-- Reference: https://github.com/kowainik/co-log/blob/45d5dd6c33f722628a0f9c10e4297f5e11ede20f/co-log/tutorials/Main.hs

ps5 :: IO ()
ps5 = do
  void $ loadFile defaultConfig
  logFilePath <- getEnv "LOG_FILE_PATH"
  checkIntervalMicroseconds <- getEnv "CHECK_INTERVAL_MICROSECONDS"
  withLogTextFile logFilePath $ \logTextFile -> do
    let loggingAction :: LogAction IO Text
        loggingAction = logTextStdout <> logTextFile

    let richMessageAction :: LogAction IO (RichMessage IO)
        richMessageAction = cmapM fmtRichMessageDefault loggingAction

    let logFieldMap :: TypeRepMap (MessageField IO)
        logFieldMap = (delete @"threadId") defaultFieldMap -- Remove thread ID field (not used)
    let messageAction :: LogAction IO Message
        messageAction = upgradeMessageAction logFieldMap richMessageAction

    usingLoggerT messageAction (ps5check (read checkIntervalMicroseconds :: Int))
