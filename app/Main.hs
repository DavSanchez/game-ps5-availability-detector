module Main where

import Colog (cmap, fmtMessage, logTextStdout, usingLoggerT)
import Control.Monad (forever)
import PS5 (ps5check)

main :: IO ()
main = forever ps5
  where
    action = cmap fmtMessage logTextStdout
    ps5 = usingLoggerT action ps5check
