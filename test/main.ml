open OUnit2
open Game
open Board
open Player
open Helper

(* Testing file for Scrabble *)

(** NOTE: This built-in function was imported from CS 3110 A2's release code. We
    represent the letter_bank as a multiset bag.

    [cmp_bag_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent bag-like lists. That means checking that they they contain the
    same elements with the same number of repetitions, though not necessarily in
    the same order. *)

let cmp_bag_like_lists lst1 lst2 =
  let sort1 = List.sort compare lst1 in
  let sort2 = List.sort compare lst2 in
  sort1 = sort2

let mini_bank = ScrabbleBoard.init_letter_bank [ 'A'; 'A'; 'B'; 'Z'; 'C' ]
let mini_bank2 = ScrabbleBoard.init_letter_bank [ 'B'; 'C'; 'E'; 'E'; 'A'; 'B' ]
let bank_letter = ScrabbleBoard.init_letter_bank [ 'D' ]
let def_bank = ScrabbleBoard.init_letter_bank []

(*helper function to turn a (string * string) list into a string list*)
let rec to_string_list (lst : (string * string) list) : string list =
  match lst with
  | [] -> []
  | (x, y) :: t -> x :: y :: to_string_list t

(* Pretty printer for char lists. *)
let rec pp_char_list (bank : char list) : string =
  match bank with
  | [] -> ""
  | h :: [] -> "'" ^ String.make 1 h ^ "'"
  | h :: t -> "'" ^ String.make 1 h ^ "'" ^ ", " ^ pp_char_list t

(* Pretty printer for string lists. *)
let rec pp_string_list (bank : string list) : string =
  match bank with
  | [] -> ""
  | h :: [] -> "" ^ h ^ ""
  | h :: t -> "" ^ h ^ "" ^ ", " ^ pp_string_list t

(* Helper testing function for init_letter_bank *)
let init_bank_test out in1 _ =
  assert_equal ~cmp:cmp_bag_like_lists ~printer:pp_char_list out
    (ScrabbleBoard.to_list_bank (ScrabbleBoard.init_letter_bank in1))

(* Helper testing function for update_bank. *)
let update_bank_test out in1 in2 _ =
  assert_equal ~cmp:cmp_bag_like_lists ~printer:pp_char_list out
    (ScrabbleBoard.to_list_bank (ScrabbleBoard.update_bank in1 in2))

let to_list_bank_test out in1 _ =
  assert_equal ~cmp:cmp_bag_like_lists ~printer:pp_char_list out
    (ScrabbleBoard.to_list_bank in1)

let letter_points = ScrabbleBoard.init_letter_points ()

let calc_points_test out in1 _ =
  assert_equal ~printer:string_of_int out
    (ScrabbleBoard.calc_points in1 letter_points)

let check_existence_test out word loc board _ =
  assert_equal ~cmp:cmp_bag_like_lists ~printer:pp_char_list out
    (List.map fst (ScrabbleBoard.check_existence word loc board))

let created_words_test out board word loc _ =
  assert_equal ~cmp:cmp_bag_like_lists ~printer:pp_string_list out
    (ScrabbleBoard.created_words word loc board)

let board_tests =
  let board = ScrabbleBoard.init_board 7 in
  ScrabbleBoard.add_word "LEMON" (('A', 0), ('A', 4)) board 0;

  let board2 = ScrabbleBoard.init_board 8 in
  ScrabbleBoard.add_word "BYE" (('A', 3), ('C', 3)) board2 0;
  ScrabbleBoard.add_word "HI" (('F', 3), ('G', 3)) board2 0;

  let board3 = ScrabbleBoard.init_board 15 in
  ScrabbleBoard.add_word "OWE" (('E', 0), ('E', 2)) board3 0;

  let board4 = ScrabbleBoard.init_board 15 in
  ScrabbleBoard.add_word "BYE" (('A', 3), ('C', 3)) board4 0;
  ScrabbleBoard.add_word "HI" (('F', 3), ('G', 3)) board4 0;
  ScrabbleBoard.add_word "CAT" (('D', 0), ('D', 2)) board4 0;
  ScrabbleBoard.add_word "DOG" (('E', 0), ('E', 2)) board4 0;
  ScrabbleBoard.add_word "BAT" (('D', 4), ('D', 6)) board4 0;
  ScrabbleBoard.add_word "RAT" (('E', 4), ('E', 6)) board4 0;

  [
    (*check existence tests --------------------------------------------------*)
    "board, no over-lapping location or letter"
    >:: check_existence_test [ 'J'; 'A'; 'V'; 'A' ] "JAVA"
          (('D', 3), ('D', 6))
          board;
    "board, check for over-lapping same letter"
    >:: check_existence_test [ 'I'; 'M'; 'E' ] "LIME" (('A', 0), ('D', 0)) board;
    "board, all over-lapping"
    >:: check_existence_test [] "LEMON" (('A', 0), ('A', 4)) board;
    "board, no over-lapping but directly adjacent to other words"
    >:: check_existence_test [ 'S'; 'S' ] "SS" (('D', 3), ('E', 3)) board2;
    "board, no over-lapping but surrounded on all sides by other words"
    >:: check_existence_test [ 'S'; 'S' ] "SS" (('D', 3), ('E', 3)) board4;
    "board, check edge case"
    >:: check_existence_test
          [ 'H'; 'I'; 'G'; 'H'; 'N'; 'E'; 'S'; 'S' ]
          "HIGHNESS"
          (('H', 10), ('O', 10))
          board;
    "board, words with different lengths"
    >:: check_existence_test
          [ 'S'; 'C'; 'R'; 'A'; 'B'; 'B'; 'L'; 'E' ]
          "SCRABBLE"
          (('H', 4), ('N', 4))
          board;
    "board, words starting at different positions"
    >:: check_existence_test
          [ 'H'; 'E'; 'L'; 'L'; 'O' ]
          "HELLO"
          (('D', 3), ('D', 7))
          board;
    "board, words placed near edges"
    >:: check_existence_test [ 'H'; 'I' ] "HI" (('F', 0), ('G', 0)) board;
    (*calc points tests ------------------------------------------------------*)
    "Board, count points test, empty list" >:: calc_points_test 0 [];
    "Board, count points test, non-empty list"
    >:: calc_points_test 16 [ [ 'Z'; 'A'; 'N'; 'Y' ] ];
    "Board, count points test, multi char list list"
    >:: calc_points_test 4 [ [ 'A'; 'N' ]; [ 'E'; 'N' ] ];
    ( "Board, letter_points test" >:: fun _ ->
      assert_equal (ScrabbleBoard.letter_value 'Z' letter_points) 10 );
    "Board to_list_bank test, minibank"
    >:: to_list_bank_test [ 'A'; 'A'; 'B'; 'Z'; 'C' ] mini_bank;
    "Board to_list_bank test, minibank2"
    >:: to_list_bank_test [ 'B'; 'C'; 'E'; 'E'; 'A'; 'B' ] mini_bank2;
    "Board to_list_bank test, bank_letter"
    >:: to_list_bank_test [ 'D' ] bank_letter;
    "Board init_letter_bank, default bank"
    >:: init_bank_test
          [
            'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'A'; 'E'; 'E'; 'F'; 'F';
            'G'; 'G'; 'G'; 'H'; 'H'; 'I'; 'I'; 'I'; 'I'; 'I'; 'I'; 'I'; 'I';
            'I'; 'J'; 'K'; 'L'; 'L'; 'L'; 'L'; 'M'; 'M'; 'N'; 'N'; 'N'; 'N';
            'N'; 'N'; 'O'; 'O'; 'O'; 'O'; 'O'; 'O'; 'O'; 'O'; 'P'; 'Q'; 'R';
            'R'; 'R'; 'R'; 'R'; 'R'; 'S'; 'S'; 'S'; 'S'; 'T'; 'T'; 'T'; 'T';
            'T'; 'T'; 'U'; 'U'; 'U'; 'U'; 'V'; 'V'; 'W'; 'W'; 'X'; 'Y'; 'Y';
            'Z'; 'B'; 'B'; 'C'; 'C'; 'D'; 'D'; 'E'; 'E'; 'E'; 'E'; 'E'; 'E';
            'E'; 'E'; 'E'; 'E';
          ]
          [];
    "Board update_bank test, 1 letter"
    >:: update_bank_test [ 'A'; 'A'; 'B'; 'C' ] mini_bank [ 'Z' ];
    "Board update_bank test, empty sample"
    >:: update_bank_test (ScrabbleBoard.to_list_bank mini_bank) mini_bank [];
    "Board update_bank test, repeated letters"
    >:: update_bank_test [ 'B'; 'C'; 'Z' ] mini_bank [ 'A'; 'A' ];
    "Board update_bank test, remove all letters"
    >:: update_bank_test [] mini_bank [ 'A'; 'A'; 'B'; 'Z'; 'C' ];
    "Board update_bank test, try to sample letter not in the bank (expected \
     behavior is return original bank)"
    >:: update_bank_test
          (ScrabbleBoard.to_list_bank mini_bank2)
          mini_bank2 [ 'H'; 'F' ];
    (*created_words tests ----------------------------------------------------*)
    "Board created_words test, only words above"
    >:: created_words_test [] [ (0, 6) ] board [ 'S' ];
    "Board created_words test, words on left"
    >:: created_words_test [ "OHI" ] [ (1, 3); (2, 3) ] board [ 'H'; 'I' ];
    "Board created_words test, words all on one side"
    >:: created_words_test [ "EA"; "MB"; "OC"; "ND" ]
          [ (1, 1); (1, 2); (1, 3); (1, 4); (1, 5) ]
          board
          [ 'A'; 'B'; 'C'; 'D'; 'E' ];
    "Board created_words test, words on both sides"
    >:: created_words_test [ "BYESSHI" ] [ (3, 3); (4, 3) ] board2 [ 'S'; 'S' ];
    "Board created_words test, words on both sides, only want letters from one \
     side"
    >:: created_words_test [ "BYES" ] [ (3, 3) ] board2 [ 'S' ];
    "Board created_words test, two words, only want one letter from one of the \
     words"
    >:: created_words_test [ "ES" ] [ (2, 4) ] board2 [ 'S' ];
    "Board created_words test, words not connected below"
    >:: created_words_test [] [ (0, 7) ] board [ 'S' ];
    "Board created_words test, words not connected right"
    >:: created_words_test [] [ (2, 1) ] board [ 'S' ];
    "Board created_words test, words not connected diagonal"
    >:: created_words_test [] [ (1, 6) ] board [ 'S' ];
    "Board created_words test, words not connected diagonal"
    >:: created_words_test [] [ (1, 6); (1, 7) ] board [ 'S'; 'S' ];
    "Board created_words test, word surrounded on all sides"
    >:: created_words_test
          [ "BYEITHI"; "CATIBAT"; "DOGTRAT" ]
          [ (3, 3); (4, 3) ]
          board4 [ 'I'; 'T' ];
    "Board created_words test, words above and to the left"
    >:: created_words_test
          [ "HSSS"; "BRS"; "AAS"; "TTS" ]
          [ (5, 4); (5, 5); (5, 6) ]
          board4 [ 'S'; 'S'; 'S' ];
    "Board created_words test, words above and to the right"
    >:: created_words_test
          [ "ESSS"; "SBR"; "SAA"; "STT" ]
          [ (2, 4); (2, 5); (2, 6) ]
          board4 [ 'S'; 'S'; 'S' ];
    "Board created_words test, words below and to the left"
    >:: created_words_test
          [ "SSSH"; "CDS"; "AOS"; "TGS" ]
          [ (5, 0); (5, 1); (5, 2) ]
          board4 [ 'S'; 'S'; 'S' ];
    "Board created_words test, words below and to the right"
    >:: created_words_test
          [ "SSSE"; "SCD"; "SAO"; "STG" ]
          [ (2, 0); (2, 1); (2, 2) ]
          board4 [ 'S'; 'S'; 'S' ];
    (*score tests ------------------------------------------------------------*)
    ( "calculate score" >:: fun _ ->
      assert_equal 20
        (ScrabbleBoard.calc_point_w_modifiers
           [ [ 'H'; 'E'; 'L'; 'L'; 'O' ] ]
           [ 'H'; 'E'; 'L'; 'L'; 'O' ]
           [ 'H'; 'E'; 'L'; 'L'; 'O' ]
           [ (13, 1); (13, 2); (13, 3); (13, 4); (13, 5) ]
           letter_points board) );
    ( "calculate score 2" >:: fun _ ->
      assert_equal 7
        (ScrabbleBoard.calc_point_w_modifiers
           (List.map Helper.char_list_of_string [ "OWES" ])
           (Helper.char_list_of_string "OWES")
           (Helper.char_list_of_string "S")
           [ (4, 3) ]
           letter_points board3) );
    (* let new_pts = ScrabbleBoard.calc_point_w_modifiers (List.map
       Helper.char_list_of_string created_words_w_input)
       (Helper.char_list_of_string next_word) index_pos letter_points board *)
    ( "sample test, count = 0" >:: fun _ ->
      assert_equal [] (ScrabbleBoard.sample 0 mini_bank) );
    ( "sample test, empty letter bank" >:: fun _ ->
      assert_equal []
        (ScrabbleBoard.sample 3 (ScrabbleBoard.update_bank bank_letter [ 'D' ]))
    );
    ( "sample test, count = 1" >:: fun _ ->
      assert_equal [ 'D' ] (ScrabbleBoard.sample 1 bank_letter) );
  ]

module Player1 = SinglePlayer

let update_tiles_test out player tiles sampled _ =
  assert_equal ~cmp:cmp_bag_like_lists ~printer:pp_char_list out
    (Player1.current_tiles (Player1.update_tiles player tiles sampled))

let check_tiles_test out player tiles _ =
  assert_equal ~printer:string_of_bool out (Player1.check_tiles player tiles)

let update_score_test out player n _ =
  assert_equal ~printer:string_of_int out
    (Player1.score (Player1.update_score player n))

(* Helper function for testing full pipeline where player inputs a word and the
   points are calculated and added to player's score. *)
let score_word_test out player word _ =
  assert_equal ~printer:string_of_int out
    (Player1.score
       (Player1.update_score player
          (ScrabbleBoard.calc_points
             [ Helper.char_list_of_string word ]
             letter_points)))

let player1 = Player1.create_player [ 'C'; 'A'; 'M'; 'E'; 'L'; 'T' ] 0 false

let player_tests =
  [
    "Player, add word and update score test, single word creation"
    >:: score_word_test 6 player1 "MELT";
    "Player, update_score test" >:: update_score_test 6 player1 6;
    "Player, update_tiles, p1 used MELT, sampled ASDF"
    >:: update_tiles_test
          [ 'A'; 'C'; 'A'; 'S'; 'D'; 'F' ]
          player1 [ 'M'; 'E'; 'L'; 'T' ] [ 'A'; 'S'; 'D'; 'F' ];
    "Player, check_tiles, invalid tiles"
    >:: check_tiles_test false player1 [ 'Z'; 'E'; 'A'; 'L' ];
    "Player, check_tiles, valid tiles"
    >:: check_tiles_test true player1 [ 'A'; 'C'; 'L'; 'M'; 'E' ];
    ( "create_player and score test, from score 0" >:: fun _ ->
      assert_equal 0
        (Player1.score
           (Player1.create_player [ 'A'; 'B'; 'D'; 'Q'; 'M'; 'L'; 'A' ] 0 false))
    );
    ( "create_player and current tiles test, from score 0" >:: fun _ ->
      assert_equal
        [ 'A'; 'B'; 'D'; 'Q'; 'M'; 'L'; 'A' ]
        (Player1.current_tiles
           (Player1.create_player [ 'A'; 'B'; 'D'; 'Q'; 'M'; 'L'; 'A' ] 0 false))
    );
    ( "print_tiles test 1" >:: fun _ ->
      assert_equal " | A | B | D | Q | M | L | A"
        Player1.(
          create_player [ 'A'; 'B'; 'D'; 'Q'; 'M'; 'L'; 'A' ] 0 false
          |> print_tiles) );
    ( "possible words test 1" >:: fun _ ->
      assert_equal
        [ "ACT"; "AT"; "CAT"; "TA" ]
        (Player1.possible_words_from_tiles
           (Player1.create_player [ 'A'; 'C'; 'T' ] 0 true)) );
  ]

(* Helper test functions for run.ml input parsing functions. *)
let pp_loc (loc : (char * int) * (char * int)) : string =
  let fst_char = String.make 1 (fst (fst loc)) in
  let snd_char = String.make 1 (fst (snd loc)) in
  let fst_int = string_of_int (snd (fst loc)) in
  let snd_int = string_of_int (snd (snd loc)) in
  fst_char ^ fst_int ^ " - " ^ snd_char ^ snd_int

(* Helper test function for gen_loc. *)
let valid_loc = (('C', 4), ('F', 4))
let in_bounds_wrong_dir = (('F', 4), ('C', 4))

let valid_loc_length_test out in1 in2 _ =
  assert_equal out (Helper.valid_loc_length in1 in2)

let valid_dir_test out in1 _ = assert_equal out (Helper.valid_dir in1)
let loc_in_bounds_test out in1 _ = assert_equal out (Helper.loc_in_bounds in1)

let gen_loc_test out in1 _ =
  assert_equal ~printer:pp_loc out (Helper.gen_loc in1)

let run_tests =
  [
    ( "gen_loc test, assert out of bounds error" >:: fun _ ->
      assert_raises (Invalid_argument "index out of bounds") (fun () ->
          Helper.gen_loc "A1-A3") );
    "gen_loc test, single to double digits"
    >:: gen_loc_test (('K', 9), ('K', 15)) "K9 - K15";
    "gen_loc test, both double digits"
    >:: gen_loc_test (('N', 11), ('N', 15)) "N11 - N15";
    "gen_loc test, valid spaces" >:: gen_loc_test (('A', 1), ('A', 7)) "A1 - A7";
    "gen_loc test, same integer" >:: gen_loc_test (('C', 4), ('F', 4)) "C4 - F4";
    "gen_loc test, same char" >:: gen_loc_test (('D', 1), ('D', 7)) "D1 - D7";
    "loc_in_bounds test, true in bounds" >:: loc_in_bounds_test true valid_loc;
    "loc_in_bounds test, out of bounds char"
    >:: loc_in_bounds_test false (('C', 4), ('Z', 4));
    "loc_in_bounds test, out of bounds num"
    >:: loc_in_bounds_test false (('C', 7), ('C', 15));
    "loc_in_bound test, true" >:: loc_in_bounds_test true in_bounds_wrong_dir;
    "valid_dir test, true" >:: valid_dir_test true valid_loc;
    "valid_dir test, wrong dir" >:: valid_dir_test false in_bounds_wrong_dir;
    "valid_dir test, not horizontal or vertical"
    >:: valid_dir_test false (('C', 4), ('D', 6));
    "valid_dir test, everything wrong"
    >:: valid_dir_test false (('D', 2), ('A', 1));
    "valid_dir test, single letter" >:: valid_dir_test true (('C', 7), ('C', 7));
    "valid_loc_length test, right length horizontal"
    >:: valid_loc_length_test true (('A', 1), ('E', 1)) "HELLO";
    "valid_loc_length test, right length vertical"
    >:: valid_loc_length_test true (('E', 4), ('E', 7)) "MEAL";
    "valid_loc_length test, too short"
    >:: valid_loc_length_test false (('E', 4), ('E', 6)) "SCARF";
    "valid_loc_length test, too long"
    >:: valid_loc_length_test false (('A', 3), ('F', 3)) "BLUE";
    "valid_loc_length test, single letter"
    >:: valid_loc_length_test true (('D', 3), ('D', 3)) "I";
  ]

(* Helper function used when reading in letter points txt file *)
let tuple_list_test out in1 _ = assert_equal out (Helper.tuple_list in1)

(* Helper function in a variety of functions *)
let char_list_of_string_test out in1 _ =
  assert_equal ~printer:pp_char_list out (Helper.char_list_of_string in1)

(* Helper function for output of ScrabbleBoard.created_words*)
let check_created_words_test out in1 _ =
  assert_equal ~printer:string_of_bool out (Helper.check_created_words in1)

(* let reverse_string_test out in1 _ = assert_equal ~printer:(fun s -> s) out
   (Helper.reverse_string in1) *)

let helper_tests =
  [
    (* "reverse_string_test, non-empty string" >:: reverse_string_test "MONEY"
       "YENOM"; *)
    "check_created_words, invalid and valid"
    >:: check_created_words_test true [];
    "check_created_words, invalid and valid"
    >:: check_created_words_test false [ "RACECAR"; "LEMON"; "MONY" ];
    "check_created_words, invalid words"
    >:: check_created_words_test false [ "FDXFD"; "ZXN" ];
    "check_created_words, all valid"
    >:: check_created_words_test true [ "BOOK"; "MAN" ];
    "char_list_of_string, non-empty string"
    >:: char_list_of_string_test
          [ 'S'; 'C'; 'R'; 'A'; 'B'; 'B'; 'L'; 'E' ]
          "SCRABBLE";
    "char_list_of_string, non-empty string" >:: char_list_of_string_test [] "";
    "char_list_of_string, empty string" >:: char_list_of_string_test [] "";
    "tuple_list, test with two elements"
    >:: tuple_list_test [ ('H', 4); ('Z', 10) ] [ "H4"; "Z10" ];
    "tuple_list, test with one element"
    >:: tuple_list_test [ ('H', 4) ] [ "H4" ];
    "tuple_list, test with no elements" >:: tuple_list_test [] [];
    ( "list without elem (empty list)" >:: fun _ ->
      assert_equal [] (Helper.list_without_elem [] "A") );
    ( "string of char list (empty list)" >:: fun _ ->
      assert_equal "" (Helper.string_of_char_list []) );
    ( "string of char list (typical case)" >:: fun _ ->
      assert_equal "HELLO"
        (Helper.string_of_char_list [ 'H'; 'E'; 'L'; 'L'; 'O' ]) );
  ]

let test_suite =
  "Test suite for OCaml Scrabble!"
  >::: List.flatten [ board_tests; player_tests; run_tests; helper_tests ]

let () = run_test_tt_main test_suite
