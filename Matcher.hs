module Matcher where

data Friend = Profile Long
            | Id String
            deriving (Eq, Show, Read)

