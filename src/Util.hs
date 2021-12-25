module Util where

import Data.Aeson                                 ( FromJSON, Object, (.:) )
import Data.Aeson.Types                           ( Parser )
import qualified Data.Text as Text                ( pack )

-- Shorthand for accessing fields in a JSON object.
(.@) :: FromJSON a => Object -> String -> Parser a
m .@ str = m .: Text.pack str