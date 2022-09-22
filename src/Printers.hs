{-# LANGUAGE OverloadedStrings #-}
module Printers 
            ( printHangmanUI
            , printWinningPicture
            , saveGameLogs
            ) where

import qualified Data.Map as M
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import SharedTypes(SecretWord, RemainingGuesses, GameState(..))
import Logic(getDecodedSecretWord, OptionMap)

type PictureMap = M.Map RemainingGuesses [String]

printPicture :: RemainingGuesses -> IO ()
printPicture rg = do
            let tenLeft  = [" ________________ ", "|                |", "|                |", "|                |", "|                |", "|                |", "|                |", "|                |", "|________________|"]
                nineLeft    = [" ________________ ", "|                |", "|                |", "|                |", "|                |", "|                |", "|                |", "|     __________ |", "|____/__________\\|"]
                eightLeft    = [" ________________ ", "|                |", "|   ||           |", "|   ||           |", "|   ||           |", "|   ||           |", "|   ||           |", "|   ||__________ |", "|____/__________\\|"]
                sevenLeft  = [" ________________ ", "|   ________     |", "|   ||           |", "|   ||           |", "|   ||           |", "|   ||           |", "|   ||           |", "|   ||__________ |", "|____/__________\\|"]
                sixLeft   = [" ________________ ", "|   ________     |", "|   ||     |     |", "|   ||     |     |", "|   ||           |", "|   ||           |", "|   ||           |", "|   ||__________ |", "|____/__________\\|"]
                fiveLeft   = [" ________________ ", "|   ________     |", "|   ||     |     |", "|   ||     |     |", "|   ||    😐     |", "|   ||           |", "|   ||           |", "|   ||__________ |", "|____/__________\\|"]
                fourLeft    = [" ________________ ", "|   ________     |", "|   ||     |     |", "|   ||     |     |", "|   ||    😕     |", "|   ||    \\|     |", "|   ||     |     |", "|   ||__________ |", "|____/__________\\|"]
                threeLeft  = [" ________________ ", "|   ________     |", "|   ||     |     |", "|   ||     |     |", "|   ||    🙁     |", "|   ||    \\|/    |", "|   ||     |     |", "|   ||__________ |", "|____/__________\\|"]
                twoLeft  = [" ________________ ", "|   ________     |", "|   ||     |     |", "|   ||     |     |", "|   ||    😮     |", "|   ||    \\|/    |", "|   ||     |     |", "|   ||____/_____ |", "|____/__________\\|"]
                oneLeft   = [" ________________ ", "|   ________     |", "|   ||     |     |", "|   ||     |     |", "|   ||    😬     |", "|   ||    \\|/    |", "|   ||     |     |", "|   ||____/_\\___ |", "|____/__________\\|"]
                noneLeft    = [" ________________ ", "|   ________     |", "|   ||     |     |", "|   ||     |     |", "|   ||    😵     |", "|   ||    \\|/    |", "|   ||     |     |", "|   ||___ / \\ __ |", "|____/__________\\|"]
                pictureMap = M.fromList [(0, noneLeft), (1, oneLeft), (2, twoLeft), (3, threeLeft), (4, fourLeft), (5, fiveLeft), (6, sixLeft), (7, sevenLeft), (8, eightLeft), (9, nineLeft), (10, tenLeft)]
                maybePicture = M.lookup rg pictureMap
                printableList = maybe [""] id maybePicture
            putStrLn $ unlines printableList

printWinningPicture :: IO ()
printWinningPicture = do
            let picture = [" ________________ ", "|   ________     |", "|   ||           |", "|   ||           |", "|   ||    😅     |", "|   ||    \\|/    |", "|   ||     |     |", "|   ||____/_\\___ |", "|____/__________\\|"]
            putStrLn $ unlines picture

printDecodedSecretWord :: SecretWord -> OptionMap -> IO ()
printDecodedSecretWord sw optMap = putStrLn $ "      " ++  getDecodedSecretWord sw optMap

printHangmanUI :: SecretWord -> OptionMap -> RemainingGuesses -> IO ()
printHangmanUI sw optMap rg = do
                            printPicture rg
                            printDecodedSecretWord sw optMap

saveGameLogs :: [Text] -> IO ()
saveGameLogs logs = do
            TIO.appendFile "hangman_logs.txt" $ T.unlines logs
            return ()