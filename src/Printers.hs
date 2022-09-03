module Printers 
            ( printHangmanUI
            ) where

import qualified Data.Map as M
import Control.Monad.State

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

printDecodedSecretWord :: SecretWord -> OptionMap -> IO ()
printDecodedSecretWord sw optMap = putStrLn $ "      " ++  getDecodedSecretWord sw optMap

printHangmanUI :: SecretWord -> OptionMap -> RemainingGuesses -> IO ()
printHangmanUI sw optMap rg = do
                            printPicture rg
                            printDecodedSecretWord sw optMap


-- this file shouldn't be concerned with State at all...
-- printHangmanUIWS :: StateT GameState IO ()
-- printHangmanUIWS = do
--         gameState <- get
--         let rg = remainingGuesses gameState
--             optMap = optionMap gameState
--             sw = secretWord gameState
--         liftIO $ printPicture rg
--         liftIO $ printDecodedSecretWord sw optMap

                        



-- -- empty board
--  _________________ 
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |                 |
-- |_________________|

-- -- one wrong
--  ________________
-- |                |
-- |                |
-- |                |
-- |                |
-- |                |
-- |                |
-- |     __________ |
-- |____/__________\|

-- -- two wrong
--  ________________
-- |                |
-- |   ||           |
-- |   ||           |
-- |   ||           |
-- |   ||           |
-- |   ||           |
-- |   ||__________ |
-- |____/__________\|

-- -- three wrong
 
--  ________________
-- |   ________     |
-- |   ||           |
-- |   ||           |
-- |   ||           |
-- |   ||           |
-- |   ||           |
-- |   ||__________ |
-- |____/__________\|
 
--  -- four wrong
--  ________________
-- |   ________     |
-- |   ||     |     |
-- |   ||     |     |
-- |   ||           |
-- |   ||           |
-- |   ||           |
-- |   ||__________ |
-- |____/__________\|

-- -- five wrong
--  ________________
-- |   ________     |
-- |   ||     |     |
-- |   ||     |     |
-- |   ||     😐    |
-- |   ||           |
-- |   ||           |
-- |   ||__________ |
-- |____/__________\|

-- -- six wrong
--  ________________
-- |   ________     |
-- |   ||     |     |
-- |   ||     |     |
-- |   ||     😕    |
-- |   ||    \|     |
-- |   ||     |     |
-- |   ||__________ |
-- |____/__________\|

-- -- seven wrong
--  _________________
-- |   ________     |
-- |   ||     |     |
-- |   ||     |     |
-- |   ||     🙁    |
-- |   ||    \|/    |
-- |   ||     |     |
-- |   ||__________ |
-- |____/__________\|

-- -- eight wrong
--  _________________
-- |   ________     |
-- |   ||     |     |
-- |   ||     |     |
-- |   ||     😮    |
-- |   ||    \|/    |
-- |   ||     |     |
-- |   ||____/_____ |
-- |____/__________\|

-- -- nine wrong
--  _________________
-- |   ________     |
-- |   ||     |     |
-- |   ||     |     |
-- |   ||     😬    |
-- |   ||    \|/    |
-- |   ||     |     |
-- |   ||____/_\___ |
-- |____/__________\|

-- -- ten wrong
--  _________________
-- |   ________     |
-- |   ||     |     |
-- |   ||     |     |
-- |   ||     😵    |
-- |   ||    \|/    |
-- |   ||     |     |
-- |   ||___ / \ __ |
-- |____/__________\|