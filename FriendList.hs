module FriendList where

import Control.Monad                              ( MonadPlus ( mzero ) )
import Data.Aeson                                 ( FromJSON ( parseJSON ), Value ( Object ), decode, Object )
import Data.HashMap.Strict                        ( HashMap, elems )
import qualified Data.ByteString.Lazy.Char8 as BS
import Matcher                                    ( (.@), mkWishlistQuery, Friend ( Profile ) )
import Text.Parsec                                ( many1, digit )
import Text.Parsec.String                         ( Parser )

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

testBS :: BS.ByteString
testBS = BS.pack "{\"friendslist\":{\"friends\":[{\"steamid\":\"76561197961172960\",\"relationship\":\"friend\",\"friend_since\":1467226817},{\"steamid\":\"76561197963082435\",\"relationship\":\"friend\",\"friend_since\":1457408869},{\"steamid\":\"76561197966468894\",\"relationship\":\"friend\",\"friend_since\":1466323051},{\"steamid\":\"76561197972249774\",\"relationship\":\"friend\",\"friend_since\":0},{\"steamid\":\"76561197976653398\",\"relationship\":\"friend\",\"friend_since\":1465767413},{\"steamid\":\"76561197982604263\",\"relationship\":\"friend\",\"friend_since\":1470285771},{\"steamid\":\"76561197987175308\",\"relationship\":\"friend\",\"friend_since\":1424854289},{\"steamid\":\"76561197995281961\",\"relationship\":\"friend\",\"friend_since\":1527455383},{\"steamid\":\"76561197996205322\",\"relationship\":\"friend\",\"friend_since\":1464544648},{\"steamid\":\"76561197996726544\",\"relationship\":\"friend\",\"friend_since\":1528534541},{\"steamid\":\"76561197999832775\",\"relationship\":\"friend\",\"friend_since\":1456274406},{\"steamid\":\"76561198000538491\",\"relationship\":\"friend\",\"friend_since\":1471638114},{\"steamid\":\"76561198001812041\",\"relationship\":\"friend\",\"friend_since\":1300980619},{\"steamid\":\"76561198005566986\",\"relationship\":\"friend\",\"friend_since\":1456295404},{\"steamid\":\"76561198012604889\",\"relationship\":\"friend\",\"friend_since\":1349875223},{\"steamid\":\"76561198012803651\",\"relationship\":\"friend\",\"friend_since\":1262034992},{\"steamid\":\"76561198012866829\",\"relationship\":\"friend\",\"friend_since\":1271543510},{\"steamid\":\"76561198012931721\",\"relationship\":\"friend\",\"friend_since\":1250716488},{\"steamid\":\"76561198013592621\",\"relationship\":\"friend\",\"friend_since\":1284145102},{\"steamid\":\"76561198015149498\",\"relationship\":\"friend\",\"friend_since\":1300980618},{\"steamid\":\"76561198016849836\",\"relationship\":\"friend\",\"friend_since\":1547950296},{\"steamid\":\"76561198021941874\",\"relationship\":\"friend\",\"friend_since\":1267639079},{\"steamid\":\"76561198029034414\",\"relationship\":\"friend\",\"friend_since\":1300642913},{\"steamid\":\"76561198034154230\",\"relationship\":\"friend\",\"friend_since\":1525043072},{\"steamid\":\"76561198042950869\",\"relationship\":\"friend\",\"friend_since\":1458167752},{\"steamid\":\"76561198044138649\",\"relationship\":\"friend\",\"friend_since\":1569820141},{\"steamid\":\"76561198047994246\",\"relationship\":\"friend\",\"friend_since\":1387562118},{\"steamid\":\"76561198050659690\",\"relationship\":\"friend\",\"friend_since\":1513724686},{\"steamid\":\"76561198071056084\",\"relationship\":\"friend\",\"friend_since\":1430473039},{\"steamid\":\"76561198086574140\",\"relationship\":\"friend\",\"friend_since\":1413722827},{\"steamid\":\"76561198097531877\",\"relationship\":\"friend\",\"friend_since\":1456331382},{\"steamid\":\"76561198102302761\",\"relationship\":\"friend\",\"friend_since\":1536609040},{\"steamid\":\"76561198170360438\",\"relationship\":\"friend\",\"friend_since\":1548015701},{\"steamid\":\"76561198176320278\",\"relationship\":\"friend\",\"friend_since\":1507988706},{\"steamid\":\"76561198275100997\",\"relationship\":\"friend\",\"friend_since\":1514106088},{\"steamid\":\"76561198380683387\",\"relationship\":\"friend\",\"friend_since\":1547845751}]}}"

mkLinks :: FriendList -> [String]
mkLinks = map mkLink . ids

mkLink :: SteamID -> String
mkLink = mkWishlistQuery . Profile . steamID