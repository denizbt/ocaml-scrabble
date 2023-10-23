open OUnit2
open Game
open Board
open Player

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

module ScrabbleBoard = Board.ScrabbleBoard
module SinglePlayer = Player.SinglePlayer

let mini_bank = ScrabbleBoard.init_letter_bank [ 'A'; 'A'; 'B'; 'Z'; 'C' ]

(* Pretty printer for char lists. *)
let rec pp_char_list (bank : char list) : string =
  match bank with
  | [] -> ""
  | h :: [] -> "'" ^ String.make 1 h ^ "'"
  | h :: t -> "'" ^ String.make 1 h ^ "'" ^ ", " ^ pp_char_list t

(* Helper testing function for update_bank. *)
let update_bank_test out in1 in2 _ =
  assert_equal ~cmp:cmp_bag_like_lists out ~printer:pp_char_list
    (ScrabbleBoard.to_list_bank (ScrabbleBoard.update_bank in1 in2))

let board_tests =
  [
    "Board update_bank test, 1 letter"
    >:: update_bank_test [ 'A'; 'A'; 'B'; 'C' ] mini_bank [ 'Z' ];
  ]

module Player1 = SinglePlayer

let player_tests =
  [
    ( "create_player and score test, from score 0" >:: fun _ ->
      assert_equal 0
        (Player1.score
           (Player1.create_player [ 'A'; 'B'; 'D'; 'Q'; 'M'; 'L'; 'A' ] 0)) );
    ( "create_player and current tiles test, from score 0" >:: fun _ ->
      assert_equal
        [ 'A'; 'B'; 'D'; 'Q'; 'M'; 'L'; 'A' ]
        (Player1.current_tiles
           (Player1.create_player [ 'A'; 'B'; 'D'; 'Q'; 'M'; 'L'; 'A' ] 0)) );
    ( "print_tiles test 1" >:: fun _ ->
      assert_equal "ABDQMLA"
        Player1.(
          create_player [ 'A'; 'B'; 'D'; 'Q'; 'M'; 'L'; 'A' ] 0 |> print_tiles)
    );
  ]

let test_suite =
  "Test suite for OCaml Scrabble!"
  >::: List.flatten [ board_tests; player_tests ]

let () = run_test_tt_main test_suite
