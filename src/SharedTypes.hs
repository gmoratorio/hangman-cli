module SharedTypes 
                ( 
                ) where

data GameStatus = InProgress | Won | Lost deriving Show
data InputValidity = Valid | NotValid deriving (Show, Eq)