module Logic
        (
        ) where

import SharedTypes (GameStatus(..))
import Data.Char (toLower)
import Data.Maybe (fromJust, isNothing)
import Data.List (all)
import qualified Data.Map as M

type SecretWord = String

data SubmitStatus = Success | Fail
data GuessStatus = Guessed | NotGuessed deriving (Show, Eq)
data InWordStatus = InWord | NotInWord deriving Show
data Option = Option {letter :: Char, guessStatus :: GuessStatus, inWordStatus :: InWordStatus} deriving Show

type OptionMap = M.Map Char Option


generateOption :: Char -> SecretWord -> Option
generateOption c sw =
            let inWord =  if c `elem` sw then InWord else NotInWord
            in Option {letter = c, guessStatus = NotGuessed, inWordStatus = inWord}


generateOptionMap :: SecretWord -> OptionMap
generateOptionMap sw = foldr    (\char acc ->
                                    let option = generateOption char sw
                                    in M.insert char option acc
                                ) mempty ['a'..'z']

getGuessStatus :: Char -> OptionMap -> GuessStatus
getGuessStatus char optMap =
            let maybeOption =  M.lookup char optMap
            in maybe NotGuessed guessStatus maybeOption

getIsInWord :: Char -> OptionMap -> InWordStatus
getIsInWord char optMap = 
            let maybeOption =  M.lookup char optMap
            in maybe NotInWord inWordStatus maybeOption

addGuess :: Char -> OptionMap -> OptionMap
addGuess char optMap = 
            let maybeOption =  M.lookup char optMap
            in if isNothing maybeOption
                then optMap
                else
                    let prevOption = fromJust maybeOption
                        newOption = Option {letter = letter prevOption, inWordStatus = inWordStatus prevOption, guessStatus = Guessed}
                    in M.adjust (const newOption) char optMap

checkForWin :: SecretWord -> OptionMap -> GameStatus
checkForWin sw optMap = if all (\char -> getGuessStatus char optMap == Guessed) sw
                            then Won
                            else InProgress


addGuesses :: OptionMap
addGuesses =    let ogMap   = generateOptionMap "hello"
                    u1      = addGuess 'e' ogMap
                    u2      = addGuess 'l' u1
                    u3      = addGuess 'h' u2
                    u4      = addGuess 'o' u3
                in u4