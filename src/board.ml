(** The signature of the scrabble board. *)
module type Board = sig
  type letter_bank
  (** Type of letter bank *)

  type tile
  (** Type of tile *)

  type board
  (** Type of 2D board *)

  val init_board : int -> int -> board
  (** Creates a board of dimensions [w] x [h] *)

  val show_board : unit
  (** Prints the ASCII representation of the board *)

  (* val check_word_fit : string -> bool *)
  (** Check if given String [word], fits on the current [board] *)

  val sample : int -> tile list
  (** Given int [n], sample [n] letters at random from leter_bank *)

  val add_word : char list -> (char * int) * (char * int) -> bool
  (** Given list of chars and and length 2 list with starting and ed , add the
      word to the board. Return whether the addition to the board was
      successful. *)
end

(** Scrabble board *)
module ScrabbleBoard : Board = struct
  (* letter_bank is similar to a RandomBag, multiset of letters *)
  type letter_bank = string list

  (* Each tile in a board *)
  type tile =
    | Empty
    | Letter of string

  type board = tile array array

  (* how do you set the board type = to this 2D init board? *)
  (* mutable types don't make any sense ! how do you set and then use later *)
  let init_board (w : int) (h : int) = Array.make_matrix w h Empty
  let show_board = failwith "unimplemented"
  let sample (n : int) = failwith "unimplemented"
  let check_word_fit (word : string) = failwith "unimplemented"

  (** Given list of chars representing the word, and a tuple of the starting and
      ending location of word on the board. *)
  let add_word (word : char list) (location : (char * int) * (char * int)) :
      bool =
    failwith "unimplemented"
end
