{-# LANGUAGE TemplateHaskell #-}

module FriendList ( fetchFriendList, FriendList(..) ) where

import           Data.Aeson           (decode)
import           Data.Aeson.TH        (defaultOptions, deriveJSON)
import           Data.Maybe           (catMaybes, fromMaybe)
import           FriendInfo           (FriendInfo (..), fetchFriendInfo)
import           Network.HTTP.Conduit (simpleHttp)
import qualified Util                 as U
import           Util                 (ApiKey (..), SteamID (..))

newtype FriendList = FriendList { friendInfos :: [FriendInfo] }
    deriving Show

newtype FriendIds = FriendIds { ids :: [SteamID] }

newtype FriendsResponse = FriendsResponse { friends :: [SteamID] }

deriveJSON defaultOptions ''FriendsResponse

fetchFriendList :: ApiKey -> SteamID -> IO FriendList
fetchFriendList apiKey accountId = do
    let friendListPath = concat ["http://api.steampowered.com/ISteamUser/GetFriendList/v0001/?key=", key apiKey, "&steamid=", U.steamid accountId, "&relationship=friend"]
    friendsJson <- simpleHttp friendListPath
    let friendIds = fromMaybe [] (decode friendsJson :: Maybe [SteamID])
    friendInfos <- mapM (fetchFriendInfo apiKey) friendIds
    return (FriendList (catMaybes friendInfos))

-- The response returned by the 'ISteamUser/GetFriendList' endpoint.
newtype GetFriendsListResponse = GetFriendsListResponse { friendslist :: FriendsResponse }

deriveJSON defaultOptions ''GetFriendsListResponse
