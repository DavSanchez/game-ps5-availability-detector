{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game
  ( Ps5Availability (available),
    ps5AvailabilityRequest,
    isPS5Available,
  )
where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple (Request, Response, getResponseBody)

data Ps5Availability = Ps5Availability
  { available :: Bool,
    user :: String
  }
  deriving (Show, Generic, Eq, FromJSON)

ps5AvailabilityRequest :: Request
ps5AvailabilityRequest = "POST https://www.game.es/checkavailability/ps5playstation5reserva"

isPS5Available :: Ps5Availability -> Bool
isPS5Available = available
