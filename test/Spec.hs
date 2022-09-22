module Main where

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
            )

main :: IO ()
main = runTests

runTests :: IO ()    
runTests = hspec $ do
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