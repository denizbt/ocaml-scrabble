open Game
open Board
open Player
open Helper

let rec pp_list (f : 'a -> string) (lst : 'a list) =
  match lst with
  | [] -> ""
  | h :: t -> f h ^ "; " ^ pp_list f t

let print_poss_tiles (player : SinglePlayer.t) =
  print_endline
    (pp_list (fun s -> s) (SinglePlayer.possible_words_from_tiles player))

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
  let word = String.uppercase_ascii (read_line ()) in
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

let rec make_one_play (next_word : string) (loc : (char * int) * (char * int))
    (bank : ScrabbleBoard.letter_bank) (board : ScrabbleBoard.board_type)
    (player : SinglePlayer.t) (letter_points : ScrabbleBoard.letter_points) :
    ScrabbleBoard.board_type * SinglePlayer.t * ScrabbleBoard.letter_bank =
  match next_word with
  | "" -> (board, player, bank)
  | word -> (
      (* given word in dictionary and valid loc/dir, check if it fits on
         board *)
      match ScrabbleBoard.check_existence word loc board with
      | [] ->
          print_endline (word ^ " doesn't fit there on the board!");
          let word, loc = prompt_word player board in
          make_one_play word loc bank board player letter_points
      | check_existence_output ->
          let needed_tiles = List.map fst check_existence_output in
          let index_pos = List.map snd check_existence_output in
          if SinglePlayer.check_tiles player needed_tiles then
            (* Player has the necessary tiles to place this word on the board *)
            (* Now check that all new words created by puting this word in this
               location are valid *)
            let created_words =
              ScrabbleBoard.created_words board needed_tiles index_pos
            in
            let created_words_w_input =
              word :: List.filter (fun x -> x <> word) created_words
            in
            if Helper.check_created_words created_words_w_input then (
              let new_pts =
                ScrabbleBoard.calc_point_w_modifiers
                  (List.map Helper.char_list_of_string created_words_w_input)
                  (Helper.char_list_of_string next_word)
                  needed_tiles index_pos letter_points board
              in
              (* all created words are valid *)
              ScrabbleBoard.add_word next_word loc board 0;
              ScrabbleBoard.show_board board;
              let sampled =
                ScrabbleBoard.sample (List.length needed_tiles) bank
              in
              print_endline
                ("You scored " ^ string_of_int new_pts ^ " new points!");
              let new_player =
                SinglePlayer.update_score
                  (SinglePlayer.update_tiles player needed_tiles sampled)
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
              if SinglePlayer.easy_mode new_player then
                print_poss_tiles new_player
              else ();
              (board, new_player, new_bank))
            else (
              (* not all words created by this word are valid, try again! *)
              print_endline
                "Adding that word creates invalid words on the board!";
              let word, loc = prompt_word player board in
              make_one_play word loc bank board player letter_points)
          else (
            (* Player does not have the right letters to play this word *)
            print_endline
              "You don't have enough tiles in your hand to play that word!";
            let word, loc = prompt_word player board in
            make_one_play word loc bank board player letter_points))

(* Single player make_play *)
let rec make_play_single (player : SinglePlayer.t)
    (board : ScrabbleBoard.board_type) (bank : ScrabbleBoard.letter_bank)
    (letter_points : ScrabbleBoard.letter_points) : unit =
  let word, loc = prompt_word player board in
  let new_board, new_player, new_bank =
    make_one_play word loc bank board player letter_points
  in
  match word with
  | "" ->
      print_endline
        ("\nThanks for playing! Your final score was "
        ^ string_of_int (SinglePlayer.score player)
        ^ ", and here is the final board!");
      ScrabbleBoard.show_board board
  | _ -> make_play_single new_player new_board new_bank letter_points

let rec make_play_two (player1 : SinglePlayer.t) (player2 : SinglePlayer.t)
    (board : ScrabbleBoard.board_type) (bank : ScrabbleBoard.letter_bank)
    (letter_points : ScrabbleBoard.letter_points) : unit =
  print_endline ("\nIt's " ^ SinglePlayer.name player1 ^ "'s turn!");
  print_string "Here is your set of tiles: ";
  print_endline (SinglePlayer.print_tiles player1);
  if SinglePlayer.easy_mode player1 then print_poss_tiles player1 else ();
  ScrabbleBoard.show_board board;
  let word1, loc1 = prompt_word player1 board in
  let new_board, n_player1, new_bank =
    make_one_play word1 loc1 bank board player1 letter_points
  in
  print_endline ("\nIt's " ^ SinglePlayer.name player2 ^ "'s turn!");
  print_string "Here is your set of tiles: ";
  print_endline (SinglePlayer.print_tiles player2);
  if SinglePlayer.easy_mode player2 then print_poss_tiles player2 else ();
  ScrabbleBoard.show_board new_board;
  let word2, loc2 = prompt_word player2 new_board in
  let final_board, n_player2, final_bank =
    make_one_play word2 loc2 new_bank new_board player2 letter_points
  in
  match (word1, word2) with
  | "", _ | _, "" ->
      print_endline "\nThanks for playing! Here is the final board!";
      ScrabbleBoard.show_board board;
      print_endline
        (SinglePlayer.name n_player1
        ^ "'s final score was "
        ^ string_of_int (SinglePlayer.score n_player1));
      print_endline
        (SinglePlayer.name n_player2
        ^ "'s final score was "
        ^ string_of_int (SinglePlayer.score n_player2));
      if SinglePlayer.score n_player1 > SinglePlayer.score n_player2 then
        print_endline (SinglePlayer.name n_player1 ^ " has won!")
      else if SinglePlayer.score n_player1 < SinglePlayer.score n_player2 then
        print_endline (SinglePlayer.name n_player2 ^ " has won!")
      else print_endline "It's a tie!"
  | _ -> make_play_two n_player1 n_player2 final_board final_bank letter_points

let set_up_player () : string * bool =
  print_endline "Please enter your name below:";
  print_string ">>> ";
  let player_name = read_line () in
  print_endline ("\nHi " ^ player_name ^ "!");
  print_endline
    "Do you want to play in easy mode? In easy mode, we will print a list of \
     possible words that you could construct from your tiles for you (not \
     taking the board into account however). Type Y or N.";
  print_string ">>> ";
  let mode = String.uppercase_ascii (read_line ()) = "Y" in
  (player_name, mode)

let () =
  print_endline "\nWelcome to (O)Camel ScrObble!\n";
  print_endline
    "RULES:\n\
    \ - All words are case sensitive (only use uppercase characters when \
     creating words)\n\
    \ - Always enter in a valid word which you can create with a combination \
     of your tiles and the tiles which are on the board.\n\
    \ - Most of the familiar rules of scrabble apply. However, note that new \
     words do not have to be attached to existing words\n\
    \ - Any letters placed next to each other on the board must create new \
     words in the horizontal and vertical directions.\n\
    \ - Point Modifier Key: on the board you will see: \n\
    \ \t|D| for Double Word, \n\
     \t|T| for Triple word\n\
     \t|t| for triple letter\n\
     \t|d| for double letter\n\
     \t|X| is the center tile.\n\n\
    \    \n\
     Would you like to play a 1 person or 2 person game? Enter \"1\" or \"2\".";
  print_string ">>> ";
  let multiplayer = int_of_string (read_line ()) = 2 in
  let board_dim = 15 in
  let board = ScrabbleBoard.init_board board_dim in
  let letter_bank = ScrabbleBoard.init_letter_bank [] in
  let letter_points = ScrabbleBoard.init_letter_points () in
  (* single player set up *)
  if not multiplayer then (
    let player_name, mode = set_up_player () in
    let sample = ScrabbleBoard.sample 7 letter_bank in
    let new_bank = ScrabbleBoard.update_bank letter_bank sample in
    let player = SinglePlayer.create_player sample 0 player_name mode in
    print_string "Here is your first set of tiles: ";
    print_endline (SinglePlayer.print_tiles player);
    if mode then print_poss_tiles player else ();
    print_endline "And here's your empty scrabble board:";
    ScrabbleBoard.show_board board;
    make_play_single player board new_bank letter_points)
  else (
    (* multiplayer set up *)
    print_endline "Hi, Player 1:";
    let name1, mode1 = set_up_player () in
    print_endline "\nHi, Player 2:";
    let name2, mode2 = set_up_player () in
    let sample1 = ScrabbleBoard.sample 7 letter_bank in
    let player1 = SinglePlayer.create_player sample1 0 name1 mode1 in
    let new_bank = ScrabbleBoard.update_bank letter_bank sample1 in
    let sample2 = ScrabbleBoard.sample 7 new_bank in
    let player2 = SinglePlayer.create_player sample2 0 name2 mode2 in
    let final_bank = ScrabbleBoard.update_bank new_bank sample2 in
    make_play_two player1 player2 board final_bank letter_points)
