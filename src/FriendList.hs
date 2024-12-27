{-# LANGUAGE TemplateHaskell #-}

module FriendList ( fetchFriendList, FriendList(..) ) where

import           Data.Aeson           (decode)
import           Data.Aeson.TH        (defaultOptions, deriveJSON)
import           Data.List            ((\\))
import           Data.Maybe           (catMaybes, fromMaybe)
import           FriendInfo           (FriendInfo (..), fetchFriendInfo)
import           Network.HTTP.Conduit (simpleHttp)
import qualified Util                 as U
import           Util                 (ApiKey (..), SteamId (..))

newtype FriendList = FriendList { friendInfos :: [FriendInfo] }
    deriving Show

newtype FriendIds = FriendIds { ids :: [SteamId] }

newtype FriendsResponse = FriendsResponse { friends :: [SteamId] }

deriveJSON defaultOptions ''FriendsResponse

-- The response returned by the 'ISteamUser/GetFriendList' endpoint.
newtype GetFriendsListResponse = GetFriendsListResponse { friendslist :: FriendsResponse }

deriveJSON defaultOptions ''GetFriendsListResponse

fetchFriendList :: ApiKey -> SteamId -> [SteamId] -> IO FriendList
fetchFriendList apiKey accountId excluded = do
    let friendListPath = concat ["http://api.steampowered.com/ISteamUser/GetFriendList/v0001/?key=", key apiKey, "&steamid=", U.steamid accountId, "&relationship=friend"]
    putStrLn "Fetching friend list"
    friendsJson <- simpleHttp friendListPath
    let response = decode friendsJson :: Maybe GetFriendsListResponse
        friendIds = maybe [] (friends . friendslist) response \\ excluded
        numberOfFriends = length friendIds
    putStrLn (unwords ["Found" , show numberOfFriends,  "non-excluded friends"])
    friendInfos <- mapM (uncurry (fetchFriendInfoWithOutput apiKey numberOfFriends)) (zip [1 .. ] friendIds)
    return (FriendList (catMaybes friendInfos))

fetchFriendInfoWithOutput :: ApiKey -> Int -> Int -> SteamId -> IO (Maybe FriendInfo)
fetchFriendInfoWithOutput apiKey total index  steamId = do
  friendInfo <- fetchFriendInfo apiKey steamId
  let result = U.isSuccess friendInfo
  putStrLn (unwords [result, "Fetched friend info", show index, "of", show total])
  putStrLn (unwords ["  Friend info:", show friendInfo])
  return friendInfo
