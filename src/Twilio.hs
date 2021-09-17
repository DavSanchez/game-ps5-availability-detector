{-# LANGUAGE OverloadedStrings #-}

module Twilio where

-- import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import Network.HTTP.Simple (Request, setRequestBasicAuth, setRequestBodyURLEncoded, setRequestPath)
import System.Environment (getEnv, lookupEnv)

data TwilioConfig = TwilioConfig
  { smsTo :: String,
    smsFrom :: String,
    ssid :: String,
    token :: String
  }

getTwilioConfig :: IO TwilioConfig
getTwilioConfig = do
  to <- getEnv "SMS_TO"
  from <- getEnv "SMS_FROM"
  ssid <- getEnv "TWILIO_ACCOUNT_SID"
  token <- getEnv "TWILIO_AUTH_TOKEN"
  pure $ TwilioConfig to from ssid token

twilioURL :: Request
twilioURL = "POST https://api.twilio.com/"

sendSMSReq :: TwilioConfig -> ByteString -> Request
sendSMSReq cfg msg =
  setRequestPath ("2010-04-01/Accounts/" <> ssid' <> "/Messages.json") $
    setRequestBasicAuth ssid' token' $
      setRequestBodyURLEncoded
        [ ("To", smsTo'),
          ("From", smsFrom'),
          ("Body", msg)
        ]
        twilioURL
  where
    ssid' = getSSID cfg
    token' = getToken cfg
    smsTo' = getSMSTo cfg
    smsFrom' = getSMSFrom cfg

-- Attempt to use the Reader monad by slowly working towards it (perhaps this is too simple to get it?)
-- To begin, consider: sendSMSReq :: Reader TwilioConfig ByteString -> ByteString -> Request

getSSID :: TwilioConfig -> ByteString
getSSID = fromString . ssid

getToken :: TwilioConfig -> ByteString
getToken = fromString . token

getSMSTo :: TwilioConfig -> ByteString
getSMSTo = fromString . smsTo

getSMSFrom :: TwilioConfig -> ByteString
getSMSFrom = fromString . smsFrom