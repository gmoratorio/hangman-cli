{-# LANGUAGE OverloadedStrings #-}
module IOInputs 
            ( getSecretWord
            , getDifficulty
            , getPlayerNames
            ) where

import Data.Char (toLower)

import SharedTypes (PlayerName(..), SecretWord, InputValidity(..), RemainingGuesses, GuessedLetters, GameDifficulty(..), GameState(..))
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
                putStrLn "\n Sorry, your name can only contain letters and spaces. Please try again."
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
        putStrLn $ show p1 ++  ", please provide the secret word!"
        putStrLn "(letters only - no special characters)"
        sw <- getLine
        let lowerSW = toLowerString sw
            validity = checkForValidSecretWord lowerSW
        if validity == Valid
            then return lowerSW
            else do
                clearScreen
                putStrLn "\nSorry, your word was not valid. Please try again"
                getSecretWord p1

getDifficulty :: PlayerName -> IO (GameDifficulty)
getDifficulty p2 = do
            putStrLn $ show p2 ++ ", select your difficuty. Enter E for Easy, N for Normal, or H for Hard."
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
                    getDifficulty p2
        
