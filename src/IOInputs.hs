{-# LANGUAGE OverloadedStrings #-}
module IOInputs 
            ( getSecretWord
            , getUserGuess
            , getDifficulty
            ) where

import Data.Char (toLower)
import SharedTypes (SecretWord, InputValidity(..), RemainingGuesses, GuessedLetters, GameDifficulty(..))
import Logic (checkForValidSecretWord, checkForValidGuess, checkForValidDifficulty)
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
            diff <- getLine
            if length diff == 1
                then do
                    let lowerDiff = toLower $ head diff
                        validity = checkForValidDifficulty lowerDiff
                    if validity == Valid
                        then case lowerDiff of
                            'e' -> return Easy
                            'n' -> return Normal
                            'h' -> return Hard
                        else do
                            clearScreen
                            putStrLn "Sorry, please enter E for Easy, N for Normal, or H for Hard."
                            putStrLn $ "You entered: " ++ show lowerDiff
                            getDifficulty
                else do
                    clearScreen
                    putStrLn "Sorry, please enter E for Easy, N for Normal, or H for Hard."
                    putStrLn $ "You entered: " ++ show diff
                    getDifficulty

getUserGuess :: RemainingGuesses -> GuessedLetters -> IO (Char)
getUserGuess rg letters = do
        putStrLn "\nPlayer 2, guess a letter!"
        putStrLn $ "\nYou have " ++ show rg  ++ " guesses left"
        putStrLn "\nYou've guessed these letters so far: "
        putStrLn letters
        guess <- getLine -- deliberately using getLine vs getChar since getChar acts differently with cabal run vs cabal repl
        if length guess == 1
            then do
                let lowerGuess = toLower $ head guess
                    validity = checkForValidGuess lowerGuess
                if validity == Valid
                    then return lowerGuess
                    else do
                        clearScreen
                        putStrLn "Sorry, your guess must be a letter. Please try again."
                        putStrLn $ "You guessed: " ++ show guess
                        getUserGuess rg letters
            else do
                clearScreen
                putStrLn "Sorry, you can only guess one letter at a time!"
                putStrLn $ "You guessed: " ++ show guess
                getUserGuess rg letters
        
