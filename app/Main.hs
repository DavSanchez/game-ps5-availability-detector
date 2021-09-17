module Main where

import Colog (LogAction, Message, RichMessage, cmapM, defaultFieldMap, fmtRichMessageDefault, logTextStdout, upgradeMessageAction, usingLoggerT)
import Control.Monad (forever)
import PS5 (ps5check)

main :: IO ()
main = forever ps5

-- Reference: https://github.com/kowainik/co-log/blob/45d5dd6c33f722628a0f9c10e4297f5e11ede20f/co-log/tutorials/Main.hs

ps5 :: IO ()
ps5 = usingLoggerT fullMessageAction ps5check

richMessageAction :: LogAction IO (RichMessage IO)
richMessageAction = cmapM fmtRichMessageDefault logTextStdout

fullMessageAction :: LogAction IO Message
fullMessageAction = upgradeMessageAction defaultFieldMap richMessageAction