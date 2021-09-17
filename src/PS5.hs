{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module PS5 (ps5check) where

import Colog (Message, WithLog, logError, logInfo, logWarning)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Text (pack)
import GHC.Conc.IO (threadDelay)
import Game (Ps5Availability, isPS5Available, ps5AvailabilityRequest)
import Network.HTTP.Simple (Response, getResponseBody, getResponseStatus, httpJSON)
import Network.HTTP.Types.Status (Status (statusCode, statusMessage), status200)
import System.Exit (exitSuccess)
import System.IO.Error (catchIOError)
import Twilio (getTwilioConfig, sendSMSReq)

availabilityMessage :: ByteString
availabilityMessage = "\nBRO.\nLA PS5 ESTÁ DISPONIBLE EN https://www.game.es/ps5-playstation5-reserva.\n\nCORRE!"

ps5check :: (WithLog env Message m, MonadIO m) => m ()
ps5check = do
  gameRes <- httpJSON ps5AvailabilityRequest
  let status = getResponseStatus gameRes
  if status == status200
    then logInfo "[GAME.es] HTTP Response: 200 OK" >> availability gameRes
    else logError $ "[GAME.es] HTTP Response: " <> (pack . show) (statusCode status) <> " " <> (pack . show) (statusMessage status)
  liftIO $ threadDelay 60000000 -- 1 per minute
  liftIO $ pure ()

availability :: (WithLog env Message m, MonadIO m) => Response Ps5Availability -> m ()
availability g =
  logInfo ("[GAME.es] Available: " <> (pack . show) a)
    >> if a
      then do
        logWarning "[GAME.es] Looks like PS5 is available! Alerting master..."
        logInfo "[Twilio] Getting Twilio messaging info"
        twilioConf <- liftIO getTwilioConfig
        logInfo "[Twilio] Sending SMS"
        twilioResponse <- httpJSON $ sendSMSReq twilioConf availabilityMessage
        logWarning $ "[Twilio] SMS send response: " <> (pack . show) (getResponseBody twilioResponse :: Value)
        logWarning "[Twilio] Don't want to exhaust Twilio balance. Exiting..."
        liftIO exitSuccess
      else logInfo "[GAME.es] No luck. PS5 is not available. Retrying in 60 seconds." >> pure ()
  where
    a = isPS5Available g