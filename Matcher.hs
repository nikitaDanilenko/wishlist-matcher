module Matcher where

import Control.Arrow                              ( second )
import Control.Monad                              ( MonadPlus ( mzero ) )
import Control.Monad.Trans.Class                  ( lift )
import Control.Monad.Trans.Maybe                  ( MaybeT ( .. ), runMaybeT )
import Data.Aeson                                 ( FromJSON ( parseJSON ), Value ( Object ),
                                                    (.:), decode, Object )
import Data.Aeson.Types                           ( Parser )
import Data.HashMap.Strict                        ( elems )
import Data.Map                                   ( Map, fromList, (!), size )
import qualified Data.Text as Text                ( pack )
import Network.HTTP.Conduit                       ( simpleHttp, HttpException )
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List                                  ( intersect, isSuffixOf )
import Data.Maybe                                 ( maybe, fromMaybe )
import Graph.Graph                                ( GraphLL )
import Graph.MaximumMatching                      ( maximumMatching )
import Auxiliary.General                          ( Mat )
import Algebraic.Matrix                           ( fromMat, symmetricClosure, toMat )
import System.Directory                           ( getDirectoryContents )
import Data.Map                                   ( Map, fromList )
import qualified Data.Map as M                    ( lookup )

data Friend = Profile Integer
            | Id String
            deriving (Eq, Ord, Show, Read)

data Game = Game String
    deriving (Show, Eq, Ord)

data GameWish = GameWish { friend :: Friend, game :: Game }

instance FromJSON Game where
    parseJSON (Object m) = Game <$> (m .@ "name")
    parseJSON _          = mzero

data Wishlist = Wishlist [Game]
    deriving Show

instance FromJSON Wishlist where
    parseJSON (Object m) = Wishlist <$> (mapM parseJSON (elems m))

fetchWishlist :: Friend -> MaybeT IO [Game]
fetchWishlist = undefined

-- The created string can be theoretically used for querying the wishlists.
-- However, this requires a reverse proxy, since Steam is very restrictive in its CORS policy.
mkWishlistQuery :: Friend -> String
mkWishlistQuery f = prefix ++ friendQuery ++ suffix where
    prefix = "http://store.steampowered.com/wishlist/"
    suffix = "/wishlistdata/"

    friendQuery = case f of
        Profile i -> "profiles/" ++ show i
        Id name   -> "id/" ++ name

readWishlistWith :: [Game] -> BS.ByteString -> [Game]
readWishlistWith gs ws = maybe [] (intersect gs . (\(Wishlist l) -> l)) (decode ws :: Maybe Wishlist)

data WishlistGraph = WG { 
    numberedFriends :: Map Int Friend, 
    numberedGames :: Map Int Game, 
    associations :: GraphLL ()
} deriving Show

readWishlistsWith :: [Game] -> [(Friend, BS.ByteString)] -> WishlistGraph
readWishlistsWith gs fws = 
    WG (fromList (map (second fst) indexedFriends)) (fromList indexedGames) (symmetricClosure (fromMat edges)) where
        indexedFriends = zip [0 ..] fws
        numberOfFriends = length fws
        indexedGames = zip [numberOfFriends ..] gs
        gamesByName = fromList (map (\(x, y) -> (y, x)) indexedGames)
        edges = map (\(i, (f, w)) -> (i, map ((\j -> (j, ())) . (gamesByName !)) (readWishlistWith gs w))) 
                    indexedFriends

findMatching :: WishlistGraph -> [(Friend, Game)]
findMatching (WG nfs ngs graph) = result where
    matching = maximumMatching graph
    numberOfFriends = size nfs
    associations = concatMap (\(i, r) -> map (\(j, _) -> (i, j)) r) (toMat matching)
    -- We keep only "friend-to-game" associations
    subAssociations = filter (\(i, _) -> i < numberOfFriends) associations
    result = map (\(i, j) -> (nfs ! i, ngs ! j)) subAssociations

-- Shorthand for accessing fields in a JSON object.
(.@) :: FromJSON a => Object -> String -> Parser a
m .@ str = m .: Text.pack str

wishlistsFolder :: String
wishlistsFolder = "wishlists"

processFile :: String -> IO (Friend, BS.ByteString)
processFile file = do
    text <- BS.readFile (wishlistsFolder ++ "/" ++ file)
    return (Profile (read (takeWhile (/= '.') file)), text)

associationsFile :: String
associationsFile = "associations.txt"

mkAssociations :: IO (Map Friend String)
mkAssociations = do
    text <- readFile associationsFile
    let ls = lines text
        ps = map ((\(i : rest) -> (Profile (read i), unwords rest)) . words) ls
    pure (fromList ps)

main :: IO ()
main = do
    fs <- getDirectoryContents wishlistsFolder
    fbs <- mapM processFile (filter (isSuffixOf ".json") fs)
    gs <- fmap (map Game . lines) (readFile "games.txt")
    let wlg = readWishlistsWith gs fbs
        matching = findMatching wlg
    nameMap <- mkAssociations
    mapM_ (print . (\(i, (f, Game g)) -> concat [show i, ": ", show f, " (", fromMaybe "???" (M.lookup f nameMap), ")", " - ", g])) (zip [1..] matching)