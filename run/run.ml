open Game
open Board
open Player
open Helper

(** Verifies that the inputted starting position and ending position of an
    inputted word is possible / in vertical or horizontal direction. Helper
    function used in check_word*)
let rec valid_dir (loc : (char * int) * (char * int)) : bool =
  if fst (fst loc) = fst (snd loc) then true
  else if snd (fst loc) = snd (snd loc) then true
  else false

(** Given string of location (user input), checks whether the number of spots
    defined by location is the right length for the word. *)
let valid_loc_length loc word : bool =
  let fst_char = fst (fst loc) in
  let snd_char = fst (snd loc) in
  let fst_int = snd (fst loc) in
  let snd_int = snd (snd loc) in
  if fst_char = snd_char then
    if snd_int - fst_int + 1 <> String.length word then false else true
  else if fst_int = snd_int then
    if int_of_char snd_char - int_of_char fst_char + 1 <> String.length word
    then false
    else true
  else false

(** Given string of location (user input), returns a tuple of tuple
    representation of the location. *)
let gen_loc (loc : string) : (char * int) * (char * int) =
  if loc = "" then (('a', -1), ('a', -1))
  else
    let start = String.sub loc 0 2 in
    let end_ = String.sub loc 5 2 in
    ( (start.[0], int_of_char start.[1] - 48),
      (end_.[0], int_of_char end_.[1] - 48) )

(** Prompts the user to enter a location until it confirms that the format is
    valid. DOESN'T USE Board.check_word_fit right now. *)
let rec prompt_location (word : string) =
  print_endline
    "Please specify where you want to place the word (follow the format A3 - \
     A7). Or hit enter to end the game.";
  print_string ">>> ";
  let loc = gen_loc (read_line ()) in
  if snd (fst loc) = -1 || (valid_loc_length loc word && valid_dir loc) then loc
  else (
    print_endline
      "That is not a valid format or length for the location of the word. \
       Please try again.";
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
