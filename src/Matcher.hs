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
import           Data.Aeson.TH             (defaultOptions, deriveFromJSON)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Char                 (toLower)
import           Data.List                 (intercalate, intersect, isPrefixOf,
                                            isSuffixOf, (\\))
import           Data.List.Split           (splitOn)
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

data Game = Game { name :: String, appId :: String }
    deriving (Show, Eq, Ord)

data GameWish = GameWish { friend :: FriendInfo, game :: Game }

newtype WishlistItem = WishlistItem { appid :: Int }
  deriving (Show, Eq, Ord)

deriveFromJSON defaultOptions ''WishlistItem

newtype Wishlist = Wishlist { items :: [WishlistItem] }
    deriving Show

deriveFromJSON defaultOptions ''Wishlist

---- Not auto-derived, because Steam provides wishlists as a map, while we only want the map values.
--instance FromJSON Wishlist where
--    parseJSON (Object m)                   = Wishlist <$> mapM parseJSON (KM.elems m)
--    parseJSON (Array vector) | null vector = return (Wishlist [])
--    parseJSON _                            = mzero

newtype WishlistResponse = WishlistResponse { response :: Wishlist }
  deriving Show

deriveFromJSON defaultOptions ''WishlistResponse

httpGet :: String -> IO (Response LBS.ByteString)
httpGet url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  httpLbs request manager

-- The resulting can be used for querying the wishlists, but requires a login cookie in case on non-public profiles.
mkWishlistQuery :: ApiKey -> SteamID -> String
mkWishlistQuery apiKey steamId = concat [prefix, key apiKey, friendQuery] where
    prefix      = "https://api.steampowered.com/IWishlistService/GetWishlist/v1/?key="
    friendQuery = "&steamid=" ++ U.steamid steamId

fetchWishlist :: ApiKey -> SteamID -> IO (Maybe Wishlist)
fetchWishlist apiKey accountId = do
    getResponse <- httpGet (mkWishlistQuery apiKey accountId)
    return (fmap response (decode (responseBody getResponse)))

fetchWishlistWithOutput :: ApiKey -> Int -> Int -> FriendInfo -> IO (Maybe Wishlist)
fetchWishlistWithOutput apiKey total index friendInfo = do
   result <- fetchWishlist apiKey (SteamID (FI.steamid friendInfo))
   let success = U.isSuccess result
   putStrLn (unwords [success, "Queried wishlist for", FI.personaname friendInfo, concat ["(", show index, "/", show total, ")"]])
   return result

fetchWishlists :: ApiKey -> [FriendInfo] -> IO [(FriendInfo, Wishlist)]
fetchWishlists apiKey friendInfos =
  let total = length friendInfos
  in
  do
    wishlists <- mapM (uncurry(fetchWishlistWithOutput apiKey total)) (zip [1..] friendInfos)
    return (catMaybes (map (\(friendInfo, maybeWishlist) -> fmap (friendInfo,) maybeWishlist ) (zip friendInfos wishlists)))

intersectWith :: (a -> b -> Bool) -> [a] -> [b] -> [a]
intersectWith p as bs = filter (\a -> any (p a) bs) as

matchingGamesByWishlist :: [Game] -> Wishlist -> [Game]
matchingGamesByWishlist gs = intersectWith (\game item -> appId game == show (appid item)) gs . items

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
  ownId : key : _ <- getArgs
  let ownAccountId = SteamID ownId
      apiKey = ApiKey key
  excluded <- readExcluded
  friendList <- FL.fetchFriendList apiKey ownAccountId excluded
  let friendInfos = FL.friendInfos friendList
  wishlists <- fetchWishlists apiKey friendInfos
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

appIdSeparator :: String
appIdSeparator = " -> "

matchingWorkflow :: [(FriendInfo, Wishlist)] -> IO [(FriendInfo, Game)]
matchingWorkflow wishlists =
  fmap (findMatching . buildWishlistGraph wishlists . map (\line -> let (name : appId : _) = splitOn appIdSeparator line in Game name appId ) . lines) (readFile gamesFile)

assignmentWorkflow :: [(FriendInfo, Game)] -> IO ()
assignmentWorkflow matching =
  do
    putStrLn "Writing assignments to file."
    let assignments = map (\(friendInfo, Game game _) -> initial (FI.personaname friendInfo) (FI.loccountrycode friendInfo) game) matching
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
  mapM_ (putStrLn . (\(i, (friendInfo, Game name _)) -> concat [show i, ": ", FI.personaname friendInfo, " -> ", name ])) (zip [1..] matching)
