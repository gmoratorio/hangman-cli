module Main where

import IOInputs (getSecretWord, getUserGuess)
import SharedTypes(SecretWord, GuessCount, AttemptsAllowed, GameStatus(..), InWordStatus(..))
import Logic (generateOptionMap, OptionMap, addGuess, checkForWin, getIsInWord)

main :: IO()
main = playGame

playGame :: IO()
playGame = do
    sw <- getSecretWord
    putStrLn "Ok, the secret word is set. Now on to the game!"
    let optionMap = generateOptionMap sw
        guessCount = 0 :: GuessCount
        attemptsAllowed = 7 :: AttemptsAllowed
    playTurns sw optionMap guessCount attemptsAllowed
    

playTurns :: SecretWord -> OptionMap -> GuessCount -> AttemptsAllowed -> IO()
playTurns sw optMap gc aa = do
    if gc == aa
        then do
            putStrLn "Sorry! You're out of guesses :("
            putStrLn $ "The secret word was: " ++ show sw
        else do
            putStrLn $ "You have " ++ show (aa - gc) ++ " guesses left"
            guess <- getUserGuess
            let inWordStatus = getIsInWord guess optMap
                newOptMap = addGuess guess optMap
                gameStatus = checkForWin sw newOptMap
                newGuessCount = if inWordStatus == InWord then gc else gc + 1
            if gameStatus == Won
                then putStrLn $ "Congratulations! You correctly guessed the word: " ++ show sw
                else playTurns sw newOptMap newGuessCount aa