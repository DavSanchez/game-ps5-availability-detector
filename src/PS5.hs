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
import Network.HTTP.Simple (Response, getResponseBody, getResponseStatus, httpJSON, httpJSONEither, JSONException)
import Network.HTTP.Types.Status (Status (statusCode, statusMessage), status200)
import System.Exit (exitSuccess)
import System.IO.Error (catchIOError)
import Twilio (getTwilioConfig, sendSMSReq)

availabilityMessage :: ByteString
availabilityMessage = "RESERVAS DE LA PS5 ABIERTAS EN https://www.game.es/ps5-playstation5-reserva.\n\n¡CORRE!"

ps5check :: (WithLog env Message m, MonadIO m) => m ()
ps5check = do
  gameRes <- httpJSONEither ps5AvailabilityRequest
  let status = getResponseStatus gameRes
  if status == status200
    then logInfo "[GAME.es] HTTP Response: 200 OK" >> either responseParseError availability (getResponseBody gameRes)
    else logError $ "[GAME.es] HTTP Response: " <> (pack . show) (statusCode status) <> " " <> (pack . show) (statusMessage status)
  liftIO $ threadDelay 60000000 -- 1 per minute

availability :: (WithLog env Message m, MonadIO m) => Ps5Availability -> m ()
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

responseParseError :: (WithLog env Message m, MonadIO m) => JSONException  -> m ()
responseParseError e = logError $ "[GAME.es] JSON Parse Error: " <> (pack . show) e
