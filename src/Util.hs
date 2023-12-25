module Util where

import Data.Aeson                                 ( FromJSON, Object, (.:) )
import Data.Aeson.Key                             ( fromString )
import Data.Aeson.Types                           ( Parser )

-- Shorthand for accessing fields in a JSON object.
(.@) :: FromJSON a => Object -> String -> Parser a
m .@ str = m .: fromString str