open Game
open Board
open Player
open Helper

(* TODO delete when debugging is done *)
let rec pp_list (f : 'a -> string) (lst : 'a list) =
  match lst with
  | [] -> ""
  | h :: t -> f h ^ "; " ^ pp_list f t

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
      if Helper.in_dictionary word then
        match prompt_location word with
        | ('a', -1), ('a', -1) -> ("", (('a', -1), ('a', -1)))
        | loc -> (w, loc)
      else (
        (* Word is not in the dictionary. *)
        print_endline
          "That isn't a valid word (it's not in our dictionary). Please try \
           again.";
        prompt_word player board)

(* Given information about the player and the next word they want to add, makes
   the play and asks for the next word recursively until the user quits. *)
let rec make_play (next_word : string) (loc : (char * int) * (char * int))
    (bank : ScrabbleBoard.letter_bank) (board : ScrabbleBoard.board_type)
    (player : SinglePlayer.t) (letter_points : ScrabbleBoard.letter_points) =
  match next_word with
  | "" ->
      print_endline
        ("\nThanks for playing! Your final score was "
        ^ string_of_int (SinglePlayer.score player)
        ^ ", and here is the final board!");
      ScrabbleBoard.show_board board
  | word -> (
      (* given word in dictionary and valid loc/dir, check if it fits on
         board *)
      match ScrabbleBoard.check_existence word loc board with
      | [] ->
          print_endline (word ^ " doesn't fit there on the board!");
          let word, loc = prompt_word player board in
          make_play word loc bank board player letter_points
      | check_existence_output ->
          let used_tiles = List.map fst check_existence_output in
          let index_pos = List.map snd check_existence_output in
          print_endline (pp_list (fun c -> String.make 1 c) used_tiles);
          print_endline
            (pp_list
               (fun (n1, n2) ->
                 "(" ^ string_of_int n1 ^ ", " ^ string_of_int n2 ^ ")")
               index_pos);

          if SinglePlayer.check_tiles player used_tiles then (
            (* Player has the necessary tiles to place this word on the board *)
            (* Now check that all new words created by puting this word in this
               location are valid *)
            let created_words =
              ScrabbleBoard.created_words board used_tiles index_pos
            in
            (* TEMP PRINTING OUT CREATED WORDS INSIDE REPL LOOP *)
            print_endline ("CREATED " ^ pp_list (fun s -> s) created_words);
            if Helper.check_created_words created_words then (
              let new_pts =
                ScrabbleBoard.calc_point_w_modifiers
                  (List.map Helper.char_list_of_string created_words)
                  (Helper.char_list_of_string word)
                  index_pos letter_points board
              in
              (* all created words are valid *)
              ScrabbleBoard.add_word next_word loc board 0;
              ScrabbleBoard.show_board board;
              let sampled =
                ScrabbleBoard.sample (List.length used_tiles) bank
              in
              print_endline
                ("You scored " ^ string_of_int new_pts ^ " new points!");
              let new_player =
                SinglePlayer.update_score
                  (SinglePlayer.update_tiles player used_tiles sampled)
                  new_pts
                (* scrabble rules say that you get pts for every new word which
                   you created by putting those letters down *)
              in
              let new_bank = ScrabbleBoard.update_bank bank sampled in
              print_endline
                ("Your score is now "
                ^ string_of_int (SinglePlayer.score new_player)
                ^ ".");
              print_string "\nHere are your updated tiles: ";
              print_endline (SinglePlayer.print_tiles new_player);
              let word, loc = prompt_word new_player board in
              make_play word loc new_bank board new_player letter_points)
            else (
              (* not all words created by this word are valid, try again! *)
              print_endline
                "Adding that word creates invalid words on the board!";
              let word, loc = prompt_word player board in
              make_play word loc bank board player letter_points))
          else (
            (* Player does not have the right letters to play this word *)
            print_endline
              "You don't have enough tiles in your hand to play that word!";
            let word, loc = prompt_word player board in
            make_play word loc bank board player letter_points))

(* TODO : implement multi-player functionality *)
let () =
  print_endline "\nWelcome to (O)Camel Scrabble!\n";
  print_endline
    "RULES:\n\
    \ - All words are case sensitive (only use uppercase characters when \
     creating words)\n\
    \ - The familiar rules of scrabble apply.\n";
  print_endline "Please enter your player name";
  print_string ">>> ";
  let player_name = read_line () in
  print_endline ("\nHi " ^ player_name ^ "! Get ready to play :)");
  let board_dim = 15 in
  let board = ScrabbleBoard.init_board board_dim in
  let letter_bank = ScrabbleBoard.init_letter_bank [] in
  let letter_points = ScrabbleBoard.init_letter_points () in
  let player =
    SinglePlayer.create_player (ScrabbleBoard.sample 7 letter_bank) 0
  in
  print_string "Here is your first set of tiles: ";
  print_endline (SinglePlayer.print_tiles player);
  print_endline "And here's your empty scrabble board:";
  ScrabbleBoard.show_board board;
  let word, loc = prompt_word player board in
  make_play word loc letter_bank board player letter_points
