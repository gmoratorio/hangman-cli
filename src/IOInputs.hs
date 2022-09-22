{-# LANGUAGE OverloadedStrings #-}
module IOInputs 
            ( getSecretWord
            , getDifficulty
            ) where

import Data.Char (toLower)

import SharedTypes (SecretWord, InputValidity(..), RemainingGuesses, GuessedLetters, GameDifficulty(..), GameState(..))
import Logic (checkForValidSecretWord, checkForValidDifficulty)
import Util (toLowerString)
import System.Console.ANSI (clearScreen)

getSecretWord :: IO (SecretWord)
getSecretWord = do
        clearScreen
        putStrLn "Player 1, please provide the secret word!"
        putStrLn "(letters only - no special characters)"
        sw <- getLine
        let lowerSW = toLowerString sw
            validity = checkForValidSecretWord lowerSW
        if validity == Valid
            then return lowerSW
            else do
                clearScreen
                putStrLn "\nSorry, your word was not valid. Please try again"
                getSecretWord

getDifficulty :: IO (GameDifficulty)
getDifficulty = do
            putStrLn "Select your difficuty. Enter E for Easy, N for Normal, or H for Hard."
            diff <- getChar
            let lowerDiff = toLower diff
                validity = checkForValidDifficulty lowerDiff
            if validity == Valid
                then case lowerDiff of
                    'e' -> return Easy
                    'n' -> return Normal
                    'h' -> return Hard
                else do
                    clearScreen
                    putStrLn "\nSorry, please enter E for Easy, N for Normal, or H for Hard."
                    putStrLn $ "You entered: " ++ show lowerDiff
                    getDifficulty
        
