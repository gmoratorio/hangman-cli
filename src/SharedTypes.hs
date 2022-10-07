module SharedTypes 
                ( GameStatus(..)
                , PlayerName(..)
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
                , DifficultyMap
                , Option(..)
                , GuessStatus(..)
                , GameState(..)
                , GameEnv(..)
                , AppM
                ) where

import qualified Data.Map as M
import Data.Text (Text)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

type OptionMap = M.Map Char Option
type DifficultyMap = M.Map Char (GameDifficulty, AttemptsAllowed)
type SecretWord = String
type GuessCount = Int
type AttemptsAllowed = Int
type RemainingGuesses = Int
type GuessedLetters = String
type AppM = WriterT [Text] (ReaderT GameEnv (StateT GameState IO))

newtype PlayerName = PlayerName String
instance Show PlayerName where show (PlayerName str) = str

data GameDifficulty = Easy | Normal | Hard deriving (Show)
data GameStatus = InProgress | Won | Lost deriving (Show, Eq)
data InputValidity = Valid | NotValid deriving (Show, Eq)
data InWordStatus = InWord | NotInWord deriving (Show, Eq)
data Player = PlayerOne | PlayerTwo deriving (Show, Eq)
data GameState = GameState 
                    { optionMap :: OptionMap
                    , remainingGuesses :: RemainingGuesses
                    }
data GameEnv = GameEnv 
                    { secretWord :: SecretWord
                    , difficulty :: GameDifficulty
                    , player1 :: PlayerName
                    , player2 :: PlayerName
                    }

data GuessStatus = Guessed | NotGuessed deriving (Show, Eq)
data Option = Option {letter :: Char, guessStatus :: GuessStatus, inWordStatus :: InWordStatus} deriving Show