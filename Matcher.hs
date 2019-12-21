module Matcher where

data Friend = Profile Integer
            | Id String
            deriving (Eq, Show, Read)

type Game = String

data GameWish = GameWish { friend :: Friend, game :: Game }