(** the main file for our Scrabble command line game *)

let start_game = print_endline "Ready to play!"

let check_word_hello x = if x = "start" then start_game else print_endline("Not ready!")