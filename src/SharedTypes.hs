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
                , Player
                , OptionMap
                , Option(..)
                , GuessStatus(..)
                , GameState(..)
                , GameEnv(..)
                ) where

import qualified Data.Map as M

type OptionMap = M.Map Char Option
type SecretWord = String
type GuessCount = Int
type RemainingGuesses = Int
type GuessedLetters = String
type AttemptsAllowed = Int
data GameDifficulty = Easy | Normal | Hard deriving (Show)
data GameStatus = InProgress | Won | Lost deriving (Show, Eq)
data InputValidity = Valid | NotValid deriving (Show, Eq)
data InWordStatus = InWord | NotInWord deriving (Show, Eq)
data Player = PlayerOne | PlayerTwo deriving (Show, Eq)
data GameState = GameState 
                    { optionMap :: OptionMap
                    , remainingGuesses :: RemainingGuesses
                    }
data GameEnv = GameEnv {secretWord :: SecretWord}

data GuessStatus = Guessed | NotGuessed deriving (Show, Eq)
data Option = Option {letter :: Char, guessStatus :: GuessStatus, inWordStatus :: InWordStatus} deriving Show