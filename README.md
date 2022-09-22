# hangman-cli
CLI Hangman written in Haskell

## Welcome to Hangman!
### Getting Started
To get started, clone this repository, `cabal build` and then `cabal run`

### Game Rules
This is a two player game, where the First Player will enter a secret word, and the Second Player has a certain amount of attempts to guess that secret word, one letter at a time.

The game has 3 settings, Easy, Normal, and Hard. These settings dictate how many turns the Second Player will have to guess the word:
 * Easy -> 10
 * Normal -> 7
 * Hard -> 5

### Logging
hangman-cli will log records of each game that is completed. Note that only completed games will be logged. Logs will be saved to `hangman_logs.txt` in the root of the project folder. A sample game log is shown below:
```
New Game Start: 2022-09-22 20:07:30.751905 UTC
Player 1: Guillermo
Player 2: Hosky
Secret Word: "aspen"

Good guess! 'a' is in the secret word.
Press any key to continue to the next round.

Good guess! 's' is in the secret word.
Press any key to continue to the next round.

Good guess! 'e' is in the secret word.
Press any key to continue to the next round.

Good guess! 'p' is in the secret word.
Press any key to continue to the next round.

Good guess! 'n' is in the secret word.
Press any key to continue to the next round.

Congratulations! You correctly guessed the word: "aspen"
Game End: 2022-09-22 20:07:35.3766 UTC



New Game Start: 2022-09-22 20:07:47.155425 UTC
Player 1: Hosky
Player 2: Guillermo
Secret Word: "evergreen"

Sorry, 'q' is not in the secret word.
Press any key to continue to the next round.

Sorry, 'w' is not in the secret word.
Press any key to continue to the next round.

Good guess! 'e' is in the secret word.
Press any key to continue to the next round.

Good guess! 'r' is in the secret word.
Press any key to continue to the next round.

Sorry, 't' is not in the secret word.
Press any key to continue to the next round.

Sorry, 'y' is not in the secret word.
Press any key to co
```

Have fun!