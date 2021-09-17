module Main where

import Colog (LogAction, Message, RichMessage, cmapM, defaultFieldMap, fmtRichMessageDefault, logText, logTextStderr, logTextStdout, upgradeMessageAction, usingLoggerT, withLogTextFile)
import Control.Monad (forever)
import Data.Text (Text)
import PS5 (ps5check)

main :: IO ()
main = forever ps5

-- Reference: https://github.com/kowainik/co-log/blob/45d5dd6c33f722628a0f9c10e4297f5e11ede20f/co-log/tutorials/Main.hs

ps5 :: IO ()
ps5 = withLogTextFile "game-ps5-availability-detector--logs.log" $ \logTextFile -> do
  let loggingAction :: LogAction IO Text
      loggingAction = logTextStdout <> logTextStderr <> logTextFile
  let richMessageAction :: LogAction IO (RichMessage IO)
      richMessageAction = cmapM fmtRichMessageDefault loggingAction
  let fullMessageAction :: LogAction IO Message
      fullMessageAction = upgradeMessageAction defaultFieldMap richMessageAction
  usingLoggerT fullMessageAction ps5check
