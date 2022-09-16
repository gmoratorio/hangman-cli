module Spec where

import Test.Hspec

import SharedTypes(GameStatus(..), SecretWord)
import Logic(generateOptionMap, getDecodedSecretWord)

main :: IO ()
main = runTests

runTests :: IO ()    
runTests = hspec $ do
    let secretWord = "banana" :: SecretWord
        optionMap = generateOptionMap secretWord
        decodedSW = getDecodedSecretWord secretWord optionMap
    describe "Given a game with no letters guessed" $ do
        it "Should return a secret word with all blanks" $ do
            decodedSW `shouldBe` "______"