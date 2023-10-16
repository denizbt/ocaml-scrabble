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
  (** Print the ASCII representation of the board *)

  (* val check_word_fit : string -> bool *)
  (** Check if given String [word], fits on the current [board] *)

  val sample : int -> tile list
  (** Given int [n], sample [n] letters at random from leter_bank *)

  val add_word : char list -> (char * int) * (char * int) -> bool
  (** Given list of chars and and length 2 list with starting and ed , add the
      word to the board. Return whether the addition to the board was
      successful. *)
end
