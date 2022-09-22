{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Char (toLower)
import Data.Text (Text, pack)
import Data.Time
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

import System.Console.ANSI (clearScreen)

import IOInputs (getSecretWord, getDifficulty, getPlayerNames)
import SharedTypes
            ( SecretWord
            , GuessCount
            , RemainingGuesses
            , GuessedLetters
            , AttemptsAllowed
            , GameStatus(..)
            , InWordStatus(..)
            , GuessStatus(..)
            , GameDifficulty(..)
            , Player(..)
            , GameState(..)
            , GameEnv(..)
            , InputValidity(..)
            , AppM
            )
import Logic 
            ( generateOptionMap
            , OptionMap
            , addGuess
            , getIsInWord
            , getGuessStatus
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
    (p1, p2) <- getPlayerNames
    putStrLn "\nOk, player names are set!"
    putStrLn "Press any key to continue."
    getChar
    sw <- getSecretWord p1
    clearScreen
    putStrLn "\nOk, the secret word is set!"
    diff <- getDifficulty p2
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
        gameEnv          = GameEnv {secretWord = sw, player1 = p1, player2 = p2}
    ((_,logs), _) <- runStateT (runReaderT (runWriterT startGameAndPlayturns) gameEnv) initialGameState
    saveGameLogs logs
    return ()

printAndTell :: String -> AppM ()
printAndTell message = do
                liftIO $ putStrLn message
                tell [pack message]

startGameAndPlayturns :: AppM ()
startGameAndPlayturns = do
        startGame
        playTurns

startGame :: AppM ()
startGame = do
        env         <- ask
        let p1 = player1 env
            p2 = player2 env
            sw = secretWord env
        startDateTime <- liftIO getCurrentTime
        tell [pack $ "\nNew Game Start: " ++ show startDateTime]
        tell [pack $ "Player 1: " ++ show p1]
        tell [pack $ "Player 2: " ++ show p2]
        tell [pack $ "Secret Word: " ++ show sw]
        return ()

endGame :: GameStatus -> AppM ()
endGame gs = do
        env <- ask
        let sw = secretWord env
        if gs == Won
            then do
                liftIO printWinningPicture
                printAndTell $ "\nCongratulations! You correctly guessed the word: " ++ show sw
            else do
                printAndTell "\nSorry! You're out of guesses :("
                printAndTell $ "The secret word was: " ++ show sw
        endDateTime <- liftIO getCurrentTime
        tell [pack $ "Game End: " ++ show endDateTime ++ "\n "]


playTurns :: AppM ()
playTurns = do
        gameState   <- get
        env         <- ask
        let rg = remainingGuesses gameState
            optMap = optionMap gameState
            sw = secretWord env
            p1 = player1 env
            p2 = player2 env
        liftIO $ printHangmanUI sw optMap rg
        if rg == 0
        then endGame Lost
        else do
            guess <- lift getUserGuess
            let inWordStatus = getIsInWord guess optMap
                guessStatus = getGuessStatus guess optMap
                newOptMap = addGuess guess optMap
                newRemainingGuesses = if inWordStatus == InWord then rg else rg - 1
            if guessStatus == Guessed
                then do
                    liftIO clearScreen
                    printAndTell $ "\nHey, you already guessed " ++ show guess ++ "!"
                    printAndTell "Press any key to try again."
                    liftIO getChar
                    liftIO clearScreen
                    playTurns
                else do
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
                        then endGame Won
                        else playTurns

getUserGuess :: ReaderT GameEnv (StateT GameState IO) (Char)
getUserGuess = do
        gameState <- get
        env       <- ask
        let rg = remainingGuesses gameState
            optMap = optionMap gameState
            guessedLetters = getAllGuesses optMap
            p2 = player2 env
        liftIO $ putStrLn $ "\nYou have " ++ show rg  ++ " guesses left"
        if null guessedLetters
            then liftIO $ putStrLn "\nYou've guessed no letters so far."
            else do
                liftIO $ putStrLn "\nYou've guessed these letters so far: "
                liftIO $ putStrLn guessedLetters
        liftIO $ putStrLn $ "\n" ++ show p2 ++ ", guess a letter!\n"
        guess <- liftIO getChar
        let lowerGuess = toLower guess
            validity = checkForValidGuess lowerGuess
        if validity == Valid
            then return lowerGuess
            else do
                liftIO clearScreen
                liftIO $ putStrLn "\nSorry, your guess must be a letter. Please try again."
                liftIO $ putStrLn $ "You guessed: " ++ show guess
                getUserGuess