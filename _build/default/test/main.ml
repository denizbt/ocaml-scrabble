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

let letter_bank = ScrabbleBoard.init_letter_bank
let board_tests = []
let player_tests = []

let test_suite =
  "Test suite for OCaml Scrabble!"
  >::: List.flatten [ board_tests; player_tests ]

let () = run_test_tt_main test_suite
