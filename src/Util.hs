{-# LANGUAGE TemplateHaskell #-}

module Util where

import           Data.Aeson       (FromJSON, Object, (.:))
import           Data.Aeson.Key   (fromString)
import           Data.Aeson.TH    (defaultOptions, deriveJSON)
import           Data.Aeson.Types (Parser)

-- Shorthand for accessing fields in a JSON object.
(.@) :: FromJSON a => Object -> String -> Parser a
m .@ str = m .: fromString str

newtype ApiKey = ApiKey { key :: String }

-- The naming is consistent with the API
newtype SteamID = SteamID { steamid :: String }
    deriving Show

deriveJSON defaultOptions ''SteamID
