open Game
open Board
open Player
open Helper

(** Prompts the user to enter a location until it confirms that the format is
    valid. *)
let rec prompt_location (word : string) =
  print_endline
    "Please specify where you want to place the word (follow the format A3 - \
     A7). Or hit enter to end the game.";
  print_string ">>> ";
  let loc = gen_loc (read_line ()) in
  if
    snd (fst loc) = -1
    || (loc_in_bounds loc && valid_dir loc && valid_loc_length loc word)
  then loc
  else (
    print_endline
      "That is not a valid board coordinate, and/or correct length for the \
       word. Please try again.";
    prompt_location word)

(** Prompts the user to enter a word and location for the word. Returns tuple of
    word and location. *)
let rec prompt_word (player : SinglePlayer.t) (board : ScrabbleBoard.board_type)
    =
  print_endline "Please enter a word or hit enter to end the game.";
  print_string ">>> ";
  let word = read_line () in
  match word with
  | "" -> ("", (('a', -1), ('a', -1)))
  | w ->
      if Helper.check_word (SinglePlayer.current_tiles player) word then
        match prompt_location word with
        | ('a', -1), ('a', -1) -> ("", (('a', -1), ('a', -1)))
        | loc -> (w, loc)
      else (
        print_endline "That isn't a valid word. Please try again.";
        prompt_word player board)

(* Given information about the player and the next word they want to add, makes
   the play and asks for the next word recursively until the user quits. *)
let rec make_play (next_word : string) (loc : (char * int) * (char * int))
    (bank : ScrabbleBoard.letter_bank) (board : ScrabbleBoard.board_type)
    (player : SinglePlayer.t) =
  match next_word with
  | "" ->
      print_endline
        ("\nThanks for playing! Your final score was "
        ^ string_of_int (SinglePlayer.score player)
        ^ ", and here is the final board!");
      ScrabbleBoard.show_board board
  | word ->
      ScrabbleBoard.add_word next_word loc board 0;
      ScrabbleBoard.show_board board;
      let sampled = ScrabbleBoard.sample (String.length next_word) bank in
      let new_player = SinglePlayer.update_tiles player next_word sampled in
      let new_bank = ScrabbleBoard.update_bank bank sampled in
      print_string "\nHere are your updated tiles: ";
      print_endline (SinglePlayer.print_tiles new_player);
      let word, loc = prompt_word new_player board in
      make_play word loc new_bank board new_player

(* Only single player functionality at the moment. *)
(* TODO make it so that the scrabble board can be any dimensions? *)
let () =
  print_endline "\nWelcome to (O)Camel ScrObble!\n";
  print_endline
    "RULES:\n\
    \ - All words are case sensitive (only use uppercase characters when \
     creating words)\n\
    \ - The familiar rules of scrabble apply.\n";
  print_endline "Please enter your player name";
  print_string ">>> ";
  let player_name = read_line () in
  print_endline ("\nHi " ^ player_name ^ "! Get ready to play :)");
  let board = ScrabbleBoard.init_board 7 in
  let letter_bank = ScrabbleBoard.init_letter_bank [] in
  let player =
    SinglePlayer.create_player (ScrabbleBoard.sample 7 letter_bank) 0
  in
  print_string "Here is your first set of tiles: ";
  print_endline (SinglePlayer.print_tiles player);
  print_endline "And here's your empty scrabble board:";
  ScrabbleBoard.show_board board;
  let word, loc = prompt_word player board in
  make_play word loc letter_bank board player
