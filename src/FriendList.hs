module FriendList where

import Control.Monad                              ( MonadPlus ( mzero ) )
import Data.Aeson                                 ( FromJSON ( parseJSON ), Value ( Object ), decode, Object )
import Data.HashMap.Strict                        ( HashMap, elems )
import qualified Data.ByteString.Lazy.Char8 as BS
import Util                                       ( (.@) )

data Friend = Profile Integer
            | Id String
            deriving (Eq, Ord, Show, Read)

data FriendList = FriendList { ids :: [SteamID] }
    deriving Show

data SteamID = SteamID { steamID :: Integer }
    deriving Show

data PreList = PreList [Value]

instance FromJSON SteamID where
    parseJSON (Object m) = (SteamID . read) <$> (m .@ "steamid")

instance FromJSON FriendList where
    parseJSON (Object m) = FriendList <$> (do
        PreList pl <- PreList <$> ((m .@ "friendslist") >>= (.@ "friends"))
        mapM parseJSON pl)
    parseJSON _          = mzero

mkLinks :: FriendList -> [String]
mkLinks = map mkLink . ids

mkLink :: SteamID -> String
mkLink = mkWishlistQuery . Profile . steamID

-- The created string can be theoretically used for querying the wishlists.
-- However, this requires a reverse proxy, since Steam is very restrictive in its CORS policy.
mkWishlistQuery :: Friend -> String
mkWishlistQuery f = prefix ++ friendQuery ++ suffix where
    prefix = "http://store.steampowered.com/wishlist/"
    suffix = "/wishlistdata/"

    friendQuery = case f of
        Profile i -> "profiles/" ++ show i
        Id name   -> "id/" ++ name