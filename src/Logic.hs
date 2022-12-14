module Logic
        ( checkForValidSecretWord
        , checkForValidGuess
        , checkForValidDifficulty
        , checkForValidPlayerName
        , generateOptionMap
        , generateDefaultDifficultyMap
        , addGuess
        , getIsInWord
        , getGuessStatus
        , getAllGuesses
        , getDecodedSecretWord
        , getGameStatus
        , OptionMap
        ) where

import Data.Maybe (fromJust, isNothing)
import Data.List (all)
import Data.Text (Text)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import SharedTypes 
                ( GameStatus(..)
                , SecretWord
                , InputValidity(..)
                , InWordStatus(..)
                , Option(..)
                , OptionMap
                , DifficultyMap
                , AttemptsAllowed
                , GuessStatus(..)
                , GameState(..)
                , GameEnv(..)
                , GameDifficulty(..)
                )




generateOption :: Char -> SecretWord -> Option
generateOption c sw =
            let inWord =  if c `elem` sw then InWord else NotInWord
            in Option {letter = c, guessStatus = NotGuessed, inWordStatus = inWord}


generateOptionMap :: SecretWord -> OptionMap
generateOptionMap sw = foldr    (\char acc ->
                                    let option = generateOption char sw
                                    in M.insert char option acc
                                ) mempty ['a'..'z']

generateDefaultDifficultyMap :: DifficultyMap
generateDefaultDifficultyMap = M.fromList [('e', (Easy, 10 :: AttemptsAllowed)), ('n', (Normal, 7)), ('h', (Hard, 5))]

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
                    allLetters      = letter <$> guessedOptions
                in  allLetters

decodeLetter :: OptionMap -> Char -> Char
decodeLetter opt c = if getGuessStatus c opt == Guessed
                        then c
                        else '_'

getDecodedSecretWord :: SecretWord -> OptionMap -> String
getDecodedSecretWord sw optMap = decodeLetter optMap <$> sw

getGameStatus :: ReaderT GameEnv (StateT GameState IO) (GameStatus) 
getGameStatus = do
    game <- get
    env <- ask
    if all (\char -> getGuessStatus char (optionMap game) == Guessed) (secretWord env)
                            then return Won
                            else return InProgress

checkForValidSecretWord :: String -> InputValidity 
checkForValidSecretWord input = if all (\char -> char `elem` ['a'..'z']) input && length input >= 2
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

checkForValidPlayerName :: String -> InputValidity 
checkForValidPlayerName input = if all (\char -> char `elem` ['a'..'z']<>['A'..'Z']<>[' ']) input && not (null input)
                                    then Valid
                                    else NotValid