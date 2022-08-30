module SharedTypes 
                ( GameStatus(..)
                , SecretWord
                , GuessCount
                , RemainingGuesses
                , AttemptsAllowed
                , InputValidity(..)
                , InWordStatus(..)
                ) where

type SecretWord = String
type GuessCount = Int
type RemainingGuesses = Int
type AttemptsAllowed = Int
data GameStatus = InProgress | Won | Lost deriving (Show, Eq)
data InputValidity = Valid | NotValid deriving (Show, Eq)
data InWordStatus = InWord | NotInWord deriving (Show, Eq)