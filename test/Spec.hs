module Main where

import Test.QuickCheck
import Test.Hspec
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import SharedTypes
            ( GameStatus(..)
            , GameState(..)
            , GameEnv(..)
            , PlayerName(..)
            , InputValidity(..)
            , SecretWord
            )
import Logic
            ( generateOptionMap
            , getDecodedSecretWord
            , addGuess
            , getGameStatus
            , checkForValidSecretWord
            , checkForValidGuess
            , checkForValidPlayerName
            , checkForValidDifficulty
            )

main :: IO ()
main = do
    runUnitTests
    runPropertyTests

runUnitTests :: IO ()    
runUnitTests = hspec $ do
    let secretWord = "banana" :: SecretWord
        badSecretWord = "?banana!" :: SecretWord
        blankOptMap = generateOptionMap secretWord
        partialOptMap = addGuess 'a' blankOptMap 
        nearWonOptMap = addGuess 'b' partialOptMap
        winningOptMap = addGuess 'n' nearWonOptMap
        initialGameState    = GameState {optionMap = blankOptMap, remainingGuesses = 10}
        partialGameState    = GameState {optionMap = nearWonOptMap, remainingGuesses = 5}
        winningGameState    = GameState {optionMap = winningOptMap, remainingGuesses = 2}
        losingGameState     = GameState {optionMap = partialOptMap, remainingGuesses = 0}
        gameEnv             = GameEnv {secretWord = secretWord, player1 = (PlayerName "Guillermo"), player2 = (PlayerName "Hosky")}  
        decodedBlankSW = getDecodedSecretWord secretWord blankOptMap
        decodedPartialSW = getDecodedSecretWord secretWord partialOptMap
        decodedNearFullSW = getDecodedSecretWord secretWord nearWonOptMap
        decodedWinningSW = getDecodedSecretWord secretWord winningOptMap
    
    -- secret word decoding
    describe "Given a game with no letters guessed" $ do
        it "Should return a secret word with all blanks" $ do
            decodedBlankSW `shouldBe` "______"
    describe "Given a game with some letters guessed" $ do
        it "Should return a secret word with only guessed letters shown" $ do
            decodedPartialSW `shouldBe` "_a_a_a"
    describe "Given a game with some letters guessed" $ do
        it "Should return a secret word with only guessed letters shown" $ do
            decodedNearFullSW `shouldBe` "ba_a_a"            
    describe "Given an game where all letters have been guessed correctly" $ do
        it "Should return a secret word with all letters shown" $ do
            decodedWinningSW `shouldBe` "banana"
    
    -- validity checks
    describe "Given a secret word with all lowercase a-z characters" $ do
        it "Should indicate that this secret word is valid" $ do
            checkForValidSecretWord secretWord `shouldBe` Valid 
    describe "Given a secret word with all special characters" $ do
        it "Should indicate that this secret word is not valid" $ do
            checkForValidSecretWord badSecretWord `shouldBe` NotValid 
    describe "Given a guess that is a lowercase letter" $ do
        it "Should indicate that this guess is valid" $ do
            checkForValidGuess 'a' `shouldBe` Valid
    describe "Given a guess that is NOT a lowercase letter" $ do
        it "Should indicate that this guess is NOT valid" $ do
            checkForValidGuess '!' `shouldBe` NotValid
    describe "Given a PlayerName that is only letters and spaces" $ do
        it "Should indicate that this name is valid" $ do
            checkForValidPlayerName "Guillermo" `shouldBe` Valid
    describe "Given a guess that has special characters" $ do
        it "Should indicate that this name is NOT valid" $ do
            checkForValidPlayerName "Hosky!" `shouldBe` NotValid

-- QuickCheck tests
alphaLowercase :: String -> Bool 
alphaLowercase = all (\char -> char `elem` ['a'..'z'])

lettersAndSpaces :: String -> Bool
lettersAndSpaces = all (\char -> char `elem` ['a'..'z']<>['A'..'Z']<>[' '])

validDifficultyEntry :: Char -> Bool
validDifficultyEntry entry = entry `elem` ['e','n','h']

minLengthMet :: String -> Int -> Bool
minLengthMet xs len = length xs >= len

prop_valid_sw sw = (checkForValidSecretWord sw == Valid) == (alphaLowercase sw && minLengthMet sw 2)
prop_valid_name name = (checkForValidPlayerName name == Valid) == (lettersAndSpaces name && minLengthMet name 1)
prop_valid_difficulty entry = (checkForValidDifficulty entry == Valid) == validDifficultyEntry entry

quickTests = [prop_valid_sw, prop_valid_name]

run1000Tests :: Arbitrary a => Show a => Testable b => (a -> b) -> IO ()
run1000Tests = quickCheck . withMaxSuccess 1000

runPropertyTests :: IO ()
runPropertyTests = do 
            run1000Tests prop_valid_sw
            run1000Tests prop_valid_name
            run1000Tests prop_valid_difficulty