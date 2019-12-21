module Matcher where

import Control.Monad                              ( MonadPlus ( mzero ) )
import Control.Monad.Trans.Class                  ( lift )
import Control.Monad.Trans.Maybe                  ( MaybeT ( .. ), runMaybeT )
import Data.Aeson                                 ( FromJSON ( parseJSON ), Value ( Object ),
                                                    (.:), decode, Object )
import Data.Aeson.Types                           ( Parser )
import Data.HashMap.Strict                        ( elems )
import qualified Data.Text as Text                ( pack )
import Network.Socket                             ( withSocketsDo )
import Network.HTTP.Conduit                       ( simpleHttp, HttpException )
import GHC.IO.Encoding                            ( setLocaleEncoding, utf8 )
import qualified Data.ByteString.Lazy.Char8 as BS


data Friend = Profile Integer
            | Id String
            deriving (Eq, Show, Read)

data Game = Game String
    deriving Show

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

mkWishlistQuery :: Friend -> String
mkWishlistQuery f = prefix ++ friendQuery ++ suffix where
    prefix = "https://store.steampowered.com/wishlist/"
    suffix = "/wishlistdata/"

    friendQuery = case f of
        Profile i -> "profiles/" ++ show i
        Id name   -> "id/" ++ name

-- Shorthand for accessing fields in a JSON object.

(.@) :: FromJSON a => Object -> String -> Parser a
m .@ str = m .: Text.pack str

main :: IO ()
main = withSocketsDo $ do
    setLocaleEncoding utf8
    b <- simpleHttp (mkWishlistQuery (Profile 76561198071056084))
    putStrLn (show b)
