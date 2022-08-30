module Main where


import System.Console.ANSI (clearScreen)
import IOInputs (getSecretWord, getUserGuess)
import SharedTypes(SecretWord, GuessCount, RemainingGuesses, AttemptsAllowed, GameStatus(..), InWordStatus(..))
import Logic (generateOptionMap, OptionMap, addGuess, checkForWin, getIsInWord)

main :: IO()
main = playGame

playGame :: IO()
playGame = do
    sw <- getSecretWord
    clearScreen
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
            clearScreen
            let remainingGuesses = (aa - gc) :: RemainingGuesses
            guess <- getUserGuess remainingGuesses
            let inWordStatus = getIsInWord guess optMap
                newOptMap = addGuess guess optMap
                gameStatus = checkForWin sw newOptMap
                newGuessCount = if inWordStatus == InWord then gc else gc + 1
            if inWordStatus == InWord 
                then putStrLn $ "Good guess! " ++ show guess ++ " is in the secret word."
                else putStrLn $ "Sorry, " ++ show guess ++ " is not in the secret word."
            putStrLn "Press any key to continue to the next round."
            getChar
            if gameStatus == Won
                then putStrLn $ "Congratulations! You correctly guessed the word: " ++ show sw
                else playTurns sw newOptMap newGuessCount aa