{-# LANGUAGE TemplateHaskell #-}

module Util where

import           Data.Aeson       (FromJSON, Object, (.:))
import           Data.Aeson.Key   (fromString)
import           Data.Aeson.TH    (defaultOptions, deriveJSON)
import           Data.Aeson.Types (Parser)

newtype ApiKey = ApiKey { key :: String }

-- The naming is consistent with the API
newtype SteamID = SteamID { steamid :: String }
    deriving Show

deriveJSON defaultOptions ''SteamID
