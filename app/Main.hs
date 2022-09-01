module Main where


import System.Console.ANSI (clearScreen)
import IOInputs (getSecretWord, getUserGuess, getDifficulty)
import SharedTypes(SecretWord, GuessCount, RemainingGuesses, GuessedLetters, AttemptsAllowed, GameStatus(..), InWordStatus(..), GameDifficulty(..))
import Logic (generateOptionMap, OptionMap, addGuess, checkForWin, getIsInWord, getAllGuesses)

main :: IO()
main = playGame

playGame :: IO()
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
    playTurns sw optionMap guessCount attemptsAllowed
    

playTurns :: SecretWord -> OptionMap -> GuessCount -> AttemptsAllowed -> IO()
playTurns sw optMap gc aa = do
    if gc == aa
        then do
            putStrLn "Sorry! You're out of guesses :("
            putStrLn $ "The secret word was: " ++ show sw
        else do
            let remainingGuesses = (aa - gc) :: RemainingGuesses
                guessedLetters = getAllGuesses optMap
            guess <- getUserGuess remainingGuesses guessedLetters
            let inWordStatus = getIsInWord guess optMap
                newOptMap = addGuess guess optMap
                gameStatus = checkForWin sw newOptMap
                newGuessCount = if inWordStatus == InWord then gc else gc + 1
            if inWordStatus == InWord 
                then putStrLn $ "Good guess! " ++ show guess ++ " is in the secret word."
                else putStrLn $ "Sorry, " ++ show guess ++ " is not in the secret word."
            putStrLn "Press any key to continue to the next round."
            getChar
            clearScreen
            if gameStatus == Won
                then putStrLn $ "Congratulations! You correctly guessed the word: " ++ show sw
                else playTurns sw newOptMap newGuessCount aa



