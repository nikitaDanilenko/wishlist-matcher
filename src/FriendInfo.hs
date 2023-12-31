{-# LANGUAGE TemplateHaskell #-}

module FriendInfo where

import           Data.Aeson           (decode)
import           Data.Aeson.TH        (defaultOptions, deriveJSON)
import           Data.Maybe           (listToMaybe)
import           Network.HTTP.Conduit (simpleHttp)
import qualified Util                 as U
import           Util                 (ApiKey (..), SteamID (..))

-- The naming is that of the API.
data FriendInfo = FriendInfo {
    steamid        :: String,
    personaname    :: String,
    loccountrycode :: String
}

deriveJSON defaultOptions ''FriendInfo

newtype PlayerSummariesResponse = PlayerSummariesResponse { players :: [FriendInfo] }

deriveJSON defaultOptions ''PlayerSummariesResponse

newtype GetPlayerSummariesResponse = GetPlayerSummariesResponse { response :: PlayerSummariesResponse }

deriveJSON defaultOptions ''GetPlayerSummariesResponse

fetchFriendInfo :: ApiKey -> SteamID -> IO (Maybe FriendInfo)
fetchFriendInfo apiKey accountId = do
    let friendInfoPath = concat ["http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=", key apiKey, "&steamids=", U.steamid accountId]
    friendsJson <- simpleHttp friendInfoPath
    let friendInfoCandidates = decode friendsJson :: Maybe GetPlayerSummariesResponse
        friendInfoCandidate = friendInfoCandidates >>= (listToMaybe . players . response)
    return friendInfoCandidate
