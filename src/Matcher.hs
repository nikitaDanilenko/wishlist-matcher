{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Matcher where

import           Algebraic.Matrix           (fromMat, symmetricClosure, toMat)
import           Auxiliary.General          (Mat)
import           Control.Arrow              (second)
import           Control.Monad              (MonadPlus (mzero))
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import           Data.Aeson                 (FromJSON (parseJSON), Object,
                                             Value (Object), decode, (.:))
import qualified Data.Aeson.KeyMap          as KM (elems)
import           Data.Aeson.TH              (defaultOptions, deriveJSON)
import qualified Data.ByteString            as SBS
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (intersect, isSuffixOf)
import           Data.Map                   (Map, delete, fromList, size, (!))
import qualified Data.Map                   as M (elems, lookup)
import           Data.Maybe                 (catMaybes, fromMaybe, maybe)
import           Data.String                (fromString)
import           Data.Time.Clock            (UTCTime (..), addUTCTime,
                                             getCurrentTime)
import           FriendInfo                 (FriendInfo (..), fetchFriendInfo)
import qualified FriendInfo                 as FI
import           FriendList                 (FriendList (..))
import qualified FriendList                 as FL
import           Graph.Graph                (GraphLL)
import           Graph.MaximumMatching      (maximumMatching)
import           Network.HTTP.Conduit       (Cookie (..), HttpException,
                                             Request (..), Response,
                                             createCookieJar, httpLbs,
                                             newManager, parseRequest,
                                             responseBody, simpleHttp,
                                             tlsManagerSettings)
import           System.Directory           (getDirectoryContents)
import           System.Environment         (getArgs, withArgs)
import qualified Util                       as U
import           Util                       (ApiKey (..), SteamID (..), (.@))

newtype Game = Game { name :: String }
    deriving (Show, Eq, Ord)

deriveJSON defaultOptions ''Game

data GameWish = GameWish { friend :: FriendInfo, game :: Game }

newtype Wishlist = Wishlist { wishlistGames :: [Game] }
    deriving Show

-- Not auto-derived, because Steam provides wishlists as a map, while we only want the map values.
instance FromJSON Wishlist where
    parseJSON (Object m) = Wishlist <$> mapM parseJSON (KM.elems m)
    parseJSON _          = mzero

httpGetWithCookie :: String -> String -> IO (Response LBS.ByteString)
httpGetWithCookie cookieContent url = do
  manager <- newManager tlsManagerSettings
  preRequest <- parseRequest url
  now <- getCurrentTime
  let future = addUTCTime 864000 now
  let cookie = Cookie {
    cookie_name = fromString "steamLoginSecure",
    cookie_value = fromString cookieContent,
    cookie_expiry_time = future,
    cookie_domain = fromString "store.steampowered.com",
    cookie_path = fromString "/",
    cookie_creation_time = now,
    cookie_last_access_time = now,
    cookie_persistent = False,
    cookie_host_only = False,
    cookie_secure_only = False,
    cookie_http_only = False
  }
  let request = preRequest { cookieJar = Just (createCookieJar [cookie]) }
  httpLbs request manager


-- The resulting can be used for querying the wishlists, but requires a login cookie in case on non-public profiles.
mkWishlistQuery :: SteamID -> String
mkWishlistQuery steamId = concat [prefix, friendQuery, suffix] where
    prefix      = "https://store.steampowered.com/wishlist/"
    suffix      = "/wishlistdata/"
    friendQuery = "profiles/" ++ U.steamid steamId

fetchWishlist :: String -> SteamID -> IO (Maybe Wishlist)
fetchWishlist cookieContent accountId = do
    response <- httpGetWithCookie cookieContent (mkWishlistQuery accountId)
    return (decode (responseBody response))

fetchWishlists :: String -> [FriendInfo] -> IO [(FriendInfo, Wishlist)]
fetchWishlists cookieContent friendInfos = do
    wishlists <- mapM (fetchWishlist cookieContent . SteamID . FI.steamid) friendInfos
    return (catMaybes (map (\(friendInfo, maybeWishlist) -> fmap (friendInfo,) maybeWishlist ) (zip friendInfos wishlists)))

matchingGamesByWishlist :: [Game] -> Wishlist -> [Game]
matchingGamesByWishlist gs = (intersect gs . wishlistGames)

data WishlistGraph = WG {
    numberedFriends :: Map Int FriendInfo,
    numberedGames   :: Map Int Game,
    associations    :: GraphLL ()
} deriving Show

buildWishlistGraph :: [Game] -> [(FriendInfo, Wishlist)] -> WishlistGraph
buildWishlistGraph gs fws =
    WG (fromList (map (second fst) indexedFriends)) (fromList indexedGames) (symmetricClosure (fromMat edges)) where
        indexedFriends = zip [0 ..] fws
        numberOfFriends = length fws
        indexedGames = zip [numberOfFriends ..] gs
        gamesByName = fromList (map (\(x, y) -> (y, x)) indexedGames)
        edges = map (\(i, (f, w)) -> (i, map ((, ()) . (gamesByName !)) (matchingGamesByWishlist gs w))) indexedFriends

findMatching :: WishlistGraph -> [(FriendInfo, Game)]
findMatching (WG nfs ngs graph) = result where
    matching = maximumMatching graph
    numberOfFriends = size nfs
    associations = concatMap (\(i, r) -> map (\(j, _) -> (i, j)) r) (toMat matching)
    -- We keep only "friend-to-game" associations
    subAssociations = filter (\(i, _) -> i < numberOfFriends) associations
    result = map (\(i, j) -> (nfs ! i, ngs ! j)) subAssociations


deleteAll :: Ord k => [k] -> Map k a -> Map k a
deleteAll ks m = foldr delete m ks

step1 :: IO ()
step1 = do
  ownId : key : cookieContent : _ <- getArgs
  let ownAccountId = SteamID ownId
      apiKey = ApiKey key
  friendList <- FL.fetchFriendList apiKey ownAccountId
  let friendInfos = FL.friendInfos friendList
  wishlists <- fetchWishlists cookieContent friendInfos
  games <- fmap (map Game . lines) (readFile "games.txt")
  let wishlistGraph = buildWishlistGraph games wishlists
      matching = findMatching wishlistGraph
  print matching


--main :: IO ()
--main = do
----    ownId : key : _ <- getArgs
--    fs <- getDirectoryContents wishlistsFolder
--    fbs <- mapM processFile (filter (isSuffixOf ".json") fs)
--    gs <- fmap (map Game . lines) (readFile "games.txt")
--    let wlg = readWishlistsWith gs fbs
--        matching = findMatching wlg
--    nameMap <- mkAssociations
--    mapM_ (putStrLn . (\(i, (f, Game g)) -> concat [show i, ": ", show f, " (", fromMaybe "???" (M.lookup f nameMap), ")", " - ", g])) (zip [1..] matching)
--    putStrLn "unmatched: "
--    mapM_ putStrLn (M.elems (deleteAll (map fst matching) nameMap))
