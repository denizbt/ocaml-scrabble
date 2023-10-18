(** The signature of the scrabble board. *)
module type BoardType = sig
  type letter_bank
  (** Type of letter bank *)

  type tile
  (** Type of tile *)

  type board_type
  (** Type of 2D board *)

  val init_board : int -> int -> board_type
  (** Creates a board of dimensions [w] x [h] *)

  val show_board : unit
  (** Prints the ASCII representation of the board *)

  (* val check_word_fit : string -> bool *)
  (** Check if given String [word], fits on the current [board] *)

  val sample : int -> letter_bank -> tile list
  (** Given int [n], sample [n] letters at random from leter_bank. Returns empty
      list if the letter bank is empty. *)

  val add_word : string -> (char * int) * (char * int) -> bool
  (** Given string word and and length 2 list with starting and ed , add the
      word to the board. Return whether the addition to the board was
      successful. *)
end

module ScrabbleBoard : BoardType
(** Scrabble Board *)
