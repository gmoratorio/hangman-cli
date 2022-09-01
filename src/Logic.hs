module Logic
        ( checkForValidSecretWord
        , checkForValidGuess
        , checkForValidDifficulty
        , generateOptionMap
        , addGuess
        , checkForWin
        , getIsInWord
        , getAllGuesses
        , OptionMap
        ) where

import Data.Maybe (fromJust, isNothing)
import Data.List (all)
import qualified Data.Map as M

import SharedTypes (GameStatus(..), SecretWord, InputValidity(..), InWordStatus(..))

data SubmitStatus = Success | Fail
data GuessStatus = Guessed | NotGuessed deriving (Show, Eq)
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

getAllGuesses :: OptionMap -> String
getAllGuesses optMap = 
                let guessedOptions  = M.elems $ M.filter (\option -> guessStatus option == Guessed) optMap
                    allLetters      = (\option-> letter option) <$> guessedOptions
                in  allLetters

checkForWin :: SecretWord -> OptionMap -> GameStatus
checkForWin sw optMap = if all (\char -> getGuessStatus char optMap == Guessed) sw
                            then Won
                            else InProgress

checkForValidSecretWord :: String -> InputValidity 
checkForValidSecretWord input = if all (\char -> char `elem` ['a'..'z']) input
                                    then Valid
                                    else NotValid

checkForValidGuess :: Char -> InputValidity 
checkForValidGuess input = if input `elem` ['a'..'z']
                                then Valid
                                else NotValid
checkForValidDifficulty :: Char -> InputValidity 
checkForValidDifficulty input = if input `elem` ['e','n','h']
                                    then Valid
                                    else NotValid