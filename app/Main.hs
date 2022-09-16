module Main where

import Control.Monad.State
import Control.Monad.Reader
import Data.Char (toLower)

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
            , getGameStatus'
            , getAllGuesses
            , checkForValidGuess
            )
import Printers(printHangmanUI, printWinningPicture)

main :: IO()
main = playGame

playGame :: IO ()
playGame = do
    sw <- getSecretWord
    clearScreen
    putStrLn "Ok, the secret word is set!"
    diff <- getDifficulty
    clearScreen
    putStrLn $ "Ok, the difficulty is set to " ++ show diff
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
    runStateT (runReaderT playTurns gameEnv) initialGameState
    return ()

playTurns :: ReaderT GameEnv (StateT GameState IO) ()
playTurns = do
        gameState   <- get
        env         <- ask
        let rg = remainingGuesses gameState
            optMap = optionMap gameState
            sw = secretWord env
        liftIO $ printHangmanUI sw optMap rg
        if rg == 0
        then do
            liftIO $ putStrLn "Sorry! You're out of guesses :("
            liftIO $ putStrLn $ "The secret word was: " ++ show sw
        else do
            guess <- lift getUserGuess
            let inWordStatus = getIsInWord guess optMap
                newOptMap = addGuess guess optMap
                newRemainingGuesses = if inWordStatus == InWord then rg else rg - 1
            put $ gameState {optionMap = newOptMap, remainingGuesses = newRemainingGuesses}
            newGameState <- get
            -- let gameStatus = evalState (runReaderT getGameStatus env) newGameState
            gameStatus <- getGameStatus'
            liftIO clearScreen
            if inWordStatus == InWord 
                then liftIO $ putStrLn $ "Good guess! " ++ show guess ++ " is in the secret word."
                else liftIO $ putStrLn $ "Sorry, " ++ show guess ++ " is not in the secret word."
            liftIO $ putStrLn "Press any key to continue to the next round."
            liftIO getChar
            liftIO clearScreen
            if gameStatus == Won
                then do
                    liftIO printWinningPicture
                    liftIO $ putStrLn $ "Congratulations! You correctly guessed the word: " ++ show sw
                else playTurns

getUserGuess :: StateT GameState IO (Char)
getUserGuess = do
        gameState <- get
        let rg = remainingGuesses gameState
            optMap = optionMap gameState
            guessedLetters = getAllGuesses optMap
        liftIO $ putStrLn "\nPlayer 2, guess a letter!"
        liftIO $ putStrLn $ "\nYou have " ++ show rg  ++ " guesses left"
        if null guessedLetters
            then liftIO $ putStrLn "\nYou've guessed no letters so far."
            else do
                liftIO $ putStrLn "\nYou've guessed these letters so far: "
                liftIO $ putStrLn guessedLetters
        guess <- liftIO $ getLine -- deliberately using getLine vs getChar since getChar acts differently with cabal run vs cabal repl
        if length guess == 1
            then do
                let lowerGuess = toLower $ head guess
                    validity = checkForValidGuess lowerGuess
                if validity == Valid
                    then return lowerGuess
                    else do
                        liftIO $ clearScreen
                        liftIO $ putStrLn "Sorry, your guess must be a letter. Please try again."
                        liftIO $ putStrLn $ "You guessed: " ++ show guess
                        getUserGuess
            else do
                liftIO $ clearScreen
                liftIO $ putStrLn "Sorry, you can only guess one letter at a time!"
                liftIO $ putStrLn $ "You guessed: " ++ show guess
                getUserGuess