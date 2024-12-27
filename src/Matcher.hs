{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Matcher where

import           Algebraic.Matrix          (fromMat, symmetricClosure, toMat)
import           Assignment                (createMessage, initial, obfuscate)
import           Auxiliary.General         (Mat)
import           Control.Arrow             (second)
import           Control.Monad             (MonadPlus (mzero))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Aeson                (FromJSON (parseJSON),
                                            Value (Array, Object), decode)
import qualified Data.Aeson.KeyMap         as KM (elems)
import           Data.Aeson.TH             (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Char                 (toLower)
import           Data.List                 (intercalate, intersect, isPrefixOf,
                                            isSuffixOf, (\\))
import           Data.Map                  (Map, delete, fromList, size, (!))
import qualified Data.Map                  as M (elems, lookup)
import           Data.Maybe                (catMaybes, fromMaybe, maybe)
import           Data.String               (fromString)
import           Data.Time.Clock           (UTCTime (..), addUTCTime,
                                            getCurrentTime)
import           FriendInfo                (FriendInfo (..), fetchFriendInfo)
import qualified FriendInfo                as FI
import           FriendList                (FriendList (..))
import qualified FriendList                as FL
import           Graph.Graph               (GraphLL)
import           Graph.MaximumMatching     (maximumMatching)
import           Network.HTTP.Conduit      (Cookie (..), HttpException,
                                            Request (..), Response,
                                            createCookieJar, httpLbs,
                                            newManager, parseRequest,
                                            responseBody, simpleHttp,
                                            tlsManagerSettings)
import           System.Environment        (getArgs, withArgs)
import qualified Util                      as U
import           Util                      (ApiKey (..), SteamID (..))

newtype Game = Game { name :: String }
    deriving (Show, Eq, Ord)

deriveJSON defaultOptions ''Game

data GameWish = GameWish { friend :: FriendInfo, game :: Game }

newtype Wishlist = Wishlist { wishlistGames :: [Game] }
    deriving Show

-- Not auto-derived, because Steam provides wishlists as a map, while we only want the map values.
instance FromJSON Wishlist where
    parseJSON (Object m)                   = Wishlist <$> mapM parseJSON (KM.elems m)
    parseJSON (Array vector) | null vector = return (Wishlist [])
    parseJSON _                            = mzero

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
mkWishlistQuery :: SteamID -> ApiKey -> String
mkWishlistQuery steamId apiKey = concat [prefix, key apiKey friendQuery] where
    prefix      = "https://api.steampowered.com/IWishlistService/GetWishlist/v1/?key="
    friendQuery = "steamid=" ++ U.steamid steamId

fetchWishlist :: String -> SteamID -> IO (Maybe Wishlist)
fetchWishlist cookieContent accountId = do
    response <- httpGetWithCookie cookieContent (mkWishlistQuery accountId)
    return (decode (responseBody response))

fetchWishlistWithOutput :: String -> Int -> Int -> FriendInfo -> IO (Maybe Wishlist)
fetchWishlistWithOutput cookieContent total index friendInfo = do
   result <- fetchWishlist cookieContent (SteamID (FI.steamid friendInfo))
   let success = U.isSuccess result
   putStrLn (unwords [success, "Queried wishlist for", FI.personaname friendInfo, concat ["(", show index, "/", show total, ")"]])
   return result

fetchWishlists :: String -> [FriendInfo] -> IO [(FriendInfo, Wishlist)]
fetchWishlists cookieContent friendInfos =
  let total = length friendInfos
  in
  do
    wishlists <- mapM (uncurry(fetchWishlistWithOutput cookieContent total)) (zip [1..] friendInfos)
    return (catMaybes (map (\(friendInfo, maybeWishlist) -> fmap (friendInfo,) maybeWishlist ) (zip friendInfos wishlists)))

matchingGamesByWishlist :: [Game] -> Wishlist -> [Game]
matchingGamesByWishlist gs = (intersect gs . wishlistGames)

data WishlistGraph = WG {
    numberedFriends :: Map Int FriendInfo,
    numberedGames   :: Map Int Game,
    associations    :: GraphLL ()
} deriving Show

buildWishlistGraph :: [(FriendInfo, Wishlist)] -> [Game] -> WishlistGraph
buildWishlistGraph fws gs =
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

gamesFile :: String
gamesFile = "games.txt"

excludedFile :: String
excludedFile = "excluded.txt"

assignmentsFile :: String
assignmentsFile = "assignments.txt"

messagesFile :: String
messagesFile = "messages.txt"

readExcluded :: IO [SteamID]
readExcluded = do
  text <-  (readFile excludedFile)
  let excluded = (map SteamID . filter (not . isPrefixOf ['#']) . lines) text
  return excluded

branchYesNo :: String -> IO a -> IO a -> IO a
branchYesNo prefix yes no = do
  answer <- getLine
  if isPrefixOf prefix (map toLower answer) then
    yes
  else
    no



waitForDone :: IO ()
waitForDone = branchYesNo "done" (return ()) waitForDone

startWorkflow :: IO ()
startWorkflow = do
  ownId : key : cookieContent : _ <- getArgs
  let ownAccountId = SteamID ownId
      apiKey = ApiKey key
  excluded <- readExcluded
  friendList <- FL.fetchFriendList apiKey ownAccountId excluded
  let friendInfos = FL.friendInfos friendList
  wishlists <- fetchWishlists cookieContent friendInfos
  attemptToFindAssignmentWorkflow wishlists

attemptToFindAssignmentWorkflow :: [(FriendInfo, Wishlist)] -> IO ()
attemptToFindAssignmentWorkflow wishlists = do
  matching <- matchingWorkflow wishlists
  putStrLn "Here is one suggestion:"
  printMatching matching
  putStrLn "unmatched: "
  mapM_ putStrLn (map (FI.personaname . fst) wishlists \\ map (FI.personaname . fst) matching)
  putStrLn "Are you satisfied with this suggestion? If yes, type 'y', otherwise adjust the games file, and type 'n'."
  branchYesNo "y" (assignmentWorkflow matching) (attemptToFindAssignmentWorkflow wishlists)

matchingWorkflow :: [(FriendInfo, Wishlist)] -> IO [(FriendInfo, Game)]
matchingWorkflow wishlists =
  fmap (findMatching . buildWishlistGraph wishlists . map Game . lines) (readFile gamesFile)

assignmentWorkflow :: [(FriendInfo, Game)] -> IO ()
assignmentWorkflow matching =
  do
    putStrLn "Writing assignments to file."
    let assignments = map (\(friendInfo, Game game) -> initial (FI.personaname friendInfo) (FI.loccountrycode friendInfo) game) matching
    writeFile assignmentsFile (unlines (map show assignments))
    putStrLn "Written assignments to file. Please fill in the links, and then type 'done'."
    branchYesNo "done" (createMessagesWorkflow) (assignmentWorkflow matching)

createMessagesWorkflow :: IO ()
createMessagesWorkflow = do
  putStrLn "Reading assignments from file."
  assignments <- fmap (map read . lines) (readFile assignmentsFile)
  let assignmentTexts = map createMessage assignments
      outputText = intercalate "\n----------------------------------\n\n" assignmentTexts
  writeFile messagesFile outputText
  putStrLn "Written messages to file. The file will be overwritten if you run this program again."
  let obfuscatedTexts = map (show . obfuscate) assignments
      obfuscatedOutputText = unlines obfuscatedTexts
  writeFile "GiftsThisYear.txt" obfuscatedOutputText
  putStrLn "Written obfuscated messages to file. Save it for future reference. See you next year!"

printMatching :: [(FriendInfo, Game)] -> IO ()
printMatching matching = do
  mapM_ (putStrLn . (\(i, (friendInfo, Game g)) -> concat [show i, ": ", FI.personaname friendInfo, " -> ", g])) (zip [1..] matching)
