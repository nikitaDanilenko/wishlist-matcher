module Matcher where

import Control.Monad.Trans.Class                  ( lift )
import Control.Monad.Trans.Maybe                  ( MaybeT ( .. ), runMaybeT )
import Data.Aeson                                 ( FromJSON ( parseJSON ), Value ( Object ),
                                                    (.:), decode, Object )
import Data.Aeson.Types                           ( Parser )
import qualified Data.Text as Text                ( pack )


data Friend = Profile Integer
            | Id String
            deriving (Eq, Show, Read)

type Game = String

data GameWish = GameWish { friend :: Friend, game :: Game }

fetchWishList :: Friend -> MaybeT IO [Game]
fetchWishList = undefined

-- Shorthand for accessing fields in a JSON object.

(.@) :: FromJSON a => Object -> String -> Parser a
m .@ str = m .: Text.pack str