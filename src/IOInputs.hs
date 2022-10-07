module IOInputs
            ( getSecretWord
            , getDifficulty
            , getPlayerNames
            ) where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.Maybe (maybe)

import SharedTypes
            ( PlayerName(..)
            , SecretWord
            , InputValidity(..)
            , RemainingGuesses
            , GuessedLetters
            , GameDifficulty(..)
            , DifficultyMap
            , GameState(..), AttemptsAllowed
            )
import Logic (checkForValidSecretWord, checkForValidDifficulty, checkForValidPlayerName)
import Util (toLowerString)
import System.Console.ANSI (clearScreen)

getPlayerName :: IO PlayerName
getPlayerName = do
        putStrLn "\n (letters and spaces only)"
        player <- getLine
        let validity = checkForValidPlayerName player
        if validity == Valid
            then return (PlayerName player)
            else do
                putStrLn "\n Sorry, your name can only contain letters and spaces, and must be at least 1 letter long. Please try again."
                getPlayerName


getPlayerNames :: IO (PlayerName, PlayerName)
getPlayerNames = do
        clearScreen
        putStrLn "\nPlayer 1, what's your name?"
        p1 <- getPlayerName
        putStrLn "Player 2, what's your name?"
        p2 <- getPlayerName
        return (p1, p2)

getSecretWord :: PlayerName -> IO (SecretWord)
getSecretWord p1 = do
        clearScreen
        putStrLn $ show p1 <>  ", please provide the secret word!"
        putStrLn "(letters only - no special characters - at least 2 letters long)"
        sw <- getLine
        let lowerSW = toLowerString sw
            validity = checkForValidSecretWord lowerSW
        if validity == Valid
            then return lowerSW
            else do
                clearScreen
                putStrLn "\nSorry, your word was not valid. Please try again"
                getSecretWord p1

getDifficulty :: PlayerName -> DifficultyMap -> IO (GameDifficulty, AttemptsAllowed)
getDifficulty p2 dMap = do
            putStrLn $ show p2 <> ", select your difficuty. Enter E for Easy, N for Normal, or H for Hard."
            diff <- getChar
            let lowerDiff = toLower diff
                maybeDifficulty = M.lookup lowerDiff dMap
            maybe (do
                    clearScreen
                    putStrLn "\nSorry, please enter E for Easy, N for Normal, or H for Hard."
                    putStrLn $ "You entered: " <> show lowerDiff
                    getDifficulty p2 dMap) 
                    return maybeDifficulty

