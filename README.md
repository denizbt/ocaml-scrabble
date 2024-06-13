# O-Scrabble
A command-line implementation of Scrabble coded entirely in OCaml.

## How To Play
- Git Clone this repository, and run these commands.

  ```
  cd ocaml-scrabble
  make play
  ```
- Follow the instructions on your terminal, and have fun!

## Features
- Multi-Player (one-person or two-person games)
- Easy & Hard Mode
  - In easy mode, we provide a list of all valid words which you could create with the tiles in your hand (not taking into account the board).
- Double/Triple Word & Letter Tiles
- Automatic Score Calculator

Most of the familiar rules of Scrabble apply. Our interface automatically checks that every word you place on the board is in our built-in dictionary, and that every new word it creates on the board (in four cardinal directions) is valid. You are also prevented from overwriting any existing tiles on the board and are only allowed to place new letter tiles which are in your personal letter bank.


## Acknowledgements
This project was developed in Fall 2023 as a final project for CS 3110: Functional Programming and Data Structures at Cornell University.

### Developers (Cornell Netids):
- Deniz B&ouml;l&ouml;ni-Turgut (db823)
- Leane Ying (ly374)
- Alexa Sheldon (ars422)
