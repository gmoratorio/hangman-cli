module SharedTypes 
                ( GameStatus(..)
                , SecretWord
                , GuessCount
                , RemainingGuesses
                , GuessedLetters
                , AttemptsAllowed
                , GameDifficulty(..)
                , InputValidity(..)
                , InWordStatus(..)
                ) where

type SecretWord = String
type GuessCount = Int
type RemainingGuesses = Int
type GuessedLetters = String
type AttemptsAllowed = Int
data GameDifficulty = Easy | Normal | Hard deriving (Show)
data GameStatus = InProgress | Won | Lost deriving (Show, Eq)
data InputValidity = Valid | NotValid deriving (Show, Eq)
data InWordStatus = InWord | NotInWord deriving (Show, Eq)