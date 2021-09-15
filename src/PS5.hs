{-# LANGUAGE OverloadedStrings #-}

module PS5 (ps5) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Game (isPS5Available, ps5AvailabilityRequest)
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpJSON)
import Twilio (getTwilioConfig, sendSMSReq)

availabilityMessage :: ByteString
availabilityMessage = "BRO. LA PS5 ESTÁ DISPONIBLE EN https://www.game.es/ps5-playstation5-reserva. CORRE!"

ps5 :: IO ()
ps5 = do
  gameResponse <- httpJSON ps5AvailabilityRequest
  if isPS5Available gameResponse
    then do
      twilioConf <- getTwilioConfig
      twilioResponse <- httpJSON $ sendSMSReq twilioConf availabilityMessage
      print (getResponseBody twilioResponse :: Value)
    else pure ()
