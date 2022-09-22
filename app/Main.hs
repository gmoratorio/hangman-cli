{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char (toLower)
import Data.Text (Text, pack)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

import System.Console.ANSI (clearScreen)

import IOInputs (getSecretWord, getDifficulty)
import SharedTypes
            ( SecretWord
            , GuessCount
            , RemainingGuesses
            , GuessedLetters
            , AttemptsAllowed
            , GameStatus(..)
            , InWordStatus(..)
            , GameDifficulty(..)
            , Player(..)
            , GameState(..)
            , GameEnv(..)
            , InputValidity(..)
            )
import Logic 
            ( generateOptionMap
            , OptionMap
            , addGuess
            , getIsInWord
            , getGameStatus
            , getAllGuesses
            , checkForValidGuess
            )
import Printers(printHangmanUI, printWinningPicture, saveGameLogs)

main :: IO()
main = playGame

playGame :: IO ()
playGame = do
    hSetBuffering stdin NoBuffering
    sw <- getSecretWord
    clearScreen
    putStrLn "\nOk, the secret word is set!"
    diff <- getDifficulty
    clearScreen
    putStrLn $ "\nOk, the difficulty is set to " ++ show diff
    putStrLn "Now on to the game!"
    let optionMap = generateOptionMap sw
        guessCount = 0 :: GuessCount
        attemptsAllowed = case diff of
                            Easy -> 10
                            Normal -> 7
                            Hard -> 5
        remainingGuesses = attemptsAllowed :: RemainingGuesses
        initialGameState = GameState {optionMap = optionMap, remainingGuesses = remainingGuesses}
        gameEnv          = GameEnv {secretWord = sw}
    ((_,logs), _) <- runStateT (runReaderT (runWriterT playTurns) gameEnv) initialGameState
    saveGameLogs logs
    return ()

printAndTell :: String -> WriterT [Text] (ReaderT GameEnv (StateT GameState IO)) ()
printAndTell message = do
                liftIO $ putStrLn message
                tell [pack message]

playTurns :: WriterT [Text] (ReaderT GameEnv (StateT GameState IO)) ()
playTurns = do
        gameState   <- get
        env         <- ask
        let rg = remainingGuesses gameState
            optMap = optionMap gameState
            sw = secretWord env
        liftIO $ printHangmanUI sw optMap rg
        if rg == 0
        then do
            printAndTell "\nSorry! You're out of guesses :("
            printAndTell $ "The secret word was: " ++ show sw
        else do
            guess <- lift $ lift getUserGuess
            let inWordStatus = getIsInWord guess optMap
                newOptMap = addGuess guess optMap
                newRemainingGuesses = if inWordStatus == InWord then rg else rg - 1
            put $ gameState {optionMap = newOptMap, remainingGuesses = newRemainingGuesses}
            newGameState <- get
            gameStatus <- lift getGameStatus
            liftIO clearScreen
            if inWordStatus == InWord
                then printAndTell $ "\nGood guess! " ++ show guess ++ " is in the secret word."
                else printAndTell $ "\nSorry, " ++ show guess ++ " is not in the secret word."
            printAndTell "Press any key to continue to the next round."
            liftIO getChar
            liftIO clearScreen
            if gameStatus == Won
                then do
                    liftIO printWinningPicture
                    printAndTell $ "\nCongratulations! You correctly guessed the word: " ++ show sw
                else playTurns

getUserGuess :: StateT GameState IO (Char)
getUserGuess = do
        gameState <- get
        let rg = remainingGuesses gameState
            optMap = optionMap gameState
            guessedLetters = getAllGuesses optMap
        liftIO $ putStrLn $ "\nYou have " ++ show rg  ++ " guesses left"
        if null guessedLetters
            then liftIO $ putStrLn "\nYou've guessed no letters so far."
            else do
                liftIO $ putStrLn "\nYou've guessed these letters so far: "
                liftIO $ putStrLn guessedLetters
        liftIO $ putStrLn "\nPlayer 2, guess a letter!\n\n\n"
        guess <- lift getChar
        let lowerGuess = toLower guess
            validity = checkForValidGuess lowerGuess
        if validity == Valid
            then return lowerGuess
            else do
                liftIO clearScreen
                liftIO $ putStrLn "\nSorry, your guess must be a letter. Please try again."
                liftIO $ putStrLn $ "You guessed: " ++ show guess
                getUserGuess