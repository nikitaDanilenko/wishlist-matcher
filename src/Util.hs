{-# LANGUAGE TemplateHaskell #-}

module Util where

import           Data.Aeson       (FromJSON, Object, (.:))
import           Data.Aeson.Key   (fromString)
import           Data.Aeson.TH    (defaultOptions, deriveJSON)
import           Data.Aeson.Types (Parser)
import           Data.Char        (chr)

newtype ApiKey = ApiKey { key :: String }

-- The naming is consistent with the API
newtype SteamId = SteamId { steamid :: String }
    deriving (Show, Eq)

deriveJSON defaultOptions ''SteamId

-- 10004 is a checkmark, and 10008 is a cross.
isSuccess :: Maybe a -> String
isSuccess m = [maybe (chr 10008) (const (chr 10004)) m]
