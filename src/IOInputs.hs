{-# LANGUAGE OverloadedStrings #-}
module IOInputs 
            ( getSecretWord
            , getUserGuess
            ) where

import Data.Char (toLower)
import SharedTypes (SecretWord, InputValidity(..), RemainingGuesses)
import Logic (checkForValidSecretWord, checkForValidGuess)
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

getUserGuess :: RemainingGuesses -> IO (Char)
getUserGuess rg = do
        putStrLn "\nPlayer 2, guess a letter!"
        putStrLn $ "\nYou have " ++ show rg  ++ " guesses left"
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
                        getUserGuess rg
            else do
                clearScreen
                putStrLn "Sorry, you can only guess one letter at a time!"
                putStrLn $ "You guessed: " ++ show guess
                getUserGuess rg
        
