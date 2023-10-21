(** The signature of the scrabble board. *)
module type BoardType = sig
  (* type letter_bank * Type of letter bank *)

  type tile
  (** Type of tile *)

  type board_type
  (** Type of 2D board *)

  val init_board : int -> int -> board_type
  (** Creates a board of dimensions [w] x [h] *)

  (* Creates a letter_bank at the start of the game. *)

  val show_board : unit
  (** Prints the ASCII representation of the board *)

  val sample : int -> char list -> char list
  (** Given int [n], sample [n] letters at random from leter_bank. Returns empty
      list if the letter bank is empty. *)

  val add_word :
    string -> (char * int) * (char * int) -> board_type -> int -> unit
  (** Given string word and and length 2 list with starting and ed , add the
      word to the board. *)

  val check_word_fit :
    board_type -> string -> (char * int) * (char * int) -> bool
  (** Given a board, and a word and its proposed location, checks whether it
      fits on the board. *)
end

module ScrabbleBoard : BoardType
(** Scrabble Board *)
