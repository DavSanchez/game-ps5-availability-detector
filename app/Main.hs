{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Colog
  ( Limit (LimitTo),
    LogAction,
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
    withLogRotation,
    withLogTextFile,
  )
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (forever, void)
import Data.Text (Text)
import Data.TypeRepMap (TypeRepMap, delete)
import PS5 (ps5check)
import System.Environment (getEnv)
import System.Directory (removeFile)

main :: IO ()
main = forever ps5

-- Reference: https://github.com/kowainik/co-log/blob/45d5dd6c33f722628a0f9c10e4297f5e11ede20f/co-log/tutorials/Main.hs

ps5 :: IO ()
ps5 = do
  void $ loadFile defaultConfig
  logFilePath <- getEnv "LOG_FILE_PATH"
  withLogRotation (LimitTo 100000) (LimitTo 10) logFilePath removeFile loggingAction (usingLoggerT messageAction ps5check)
  where
    loggingAction :: LogAction IO Text
    loggingAction = logTextStdout <> logTextFile

    richMessageAction :: LogAction IO (RichMessage IO)
    richMessageAction = cmapM fmtRichMessageDefault loggingAction

    logFieldMap :: TypeRepMap (MessageField IO) -- Remove thread ID field (not used)
    logFieldMap = (delete @"threadId") defaultFieldMap

    messageAction :: LogAction IO Message
    messageAction = upgradeMessageAction logFieldMap richMessageAction

--usingLoggerT messageAction ps5check
