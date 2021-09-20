{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module PS5 (ps5check) where

import Colog (Message, WithLog, logError, logInfo, logWarning)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import GHC.Conc.IO (threadDelay)
import Game (Ps5Availability, isPS5Available, ps5AvailabilityRequest)
import Network.HTTP.Simple (HttpException, JSONException, Response, getResponseBody, getResponseStatus, httpJSON, httpJSONEither)
import Network.HTTP.Types.Status (Status (statusCode, statusMessage), status200)
import System.Exit (exitFailure, exitSuccess)
import Twilio (getTwilioConfig, sendSMSReq)

availabilityMessage :: ByteString
availabilityMessage = "RESERVAS DE LA PS5 ABIERTAS EN https://www.game.es/ps5-playstation5-reserva.\n\n¡CORRE!"

ps5check :: (WithLog env Message m, MonadIO m) => Int -> m ()
ps5check interval = do
  gameRes <- liftIO $ try $ httpJSONEither ps5AvailabilityRequest
  case gameRes of
    Left e -> logError ("[GAME.es] Error in request: " <> textify (e :: HttpException))
    Right res -> do
      let status = getResponseStatus res
          codeAndMessage = T.unwords [textify $ statusCode status, textify $ statusMessage status]
      if status == status200
        then logInfo ("[GAME.es] HTTP Response: " <> codeAndMessage) >> either responseParseError availability (getResponseBody res)
        else logError $ "[GAME.es] HTTP Response: " <> codeAndMessage
  liftIO $ threadDelay interval

availability :: (WithLog env Message m, MonadIO m) => Ps5Availability -> m ()
availability g =
  logInfo ("[GAME.es] Available: " <> textify a)
    >> if a
      then do
        logWarning "[GAME.es] Looks like PS5 is available! Alerting master..."
        logInfo "[Twilio] Getting Twilio messaging info"
        twilioConf <- liftIO getTwilioConfig
        logInfo "[Twilio] Sending SMS"
        twilioRes <- liftIO $ try $ httpJSON $ sendSMSReq twilioConf availabilityMessage
        case twilioRes of
          Left e -> logError ("[GAME.es] Error in request: " <> textify (e :: HttpException) <> ". Exiting...") >> liftIO exitFailure
          Right res -> do
            logWarning $ "[Twilio] SMS send response: " <> textify (getResponseBody res :: Value)
            logWarning "[Twilio] Don't want to exhaust Twilio balance. Exiting..."
            liftIO exitSuccess
      else logInfo "[GAME.es] No luck. PS5 is not available. Retrying in 60 seconds"
  where
    a = isPS5Available g

responseParseError :: (WithLog env Message m, MonadIO m) => JSONException -> m ()
responseParseError e = logError $ "[GAME.es] JSON Parse Error: " <> textify e

textify :: Show a => a -> T.Text
textify = T.pack . show
