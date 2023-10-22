(** The signature of the scrabble board. *)
module type BoardType = sig
  (* type letter_bank * Type of letter bank *)

  type tile
  (** Type of tile *)

  type board_type
  (** Type of 2D scrabble board *)

  type letter_bank
  (** Type of Scrabble's letter_bank representation. *)

  val init_board : int -> board_type
  (** Creates a board of dimensions [n] x [n] *)

  val show_board : board_type -> unit
  (** Prints the ASCII representation of the board *)

  val sample : int -> letter_bank -> char list
  (** Given int [n], and letter bank typed [bank], returns a char list of [n]
      letters sampled at random from leter_bank. Returns empty list if the
      letter bank is empty. *)

  val add_word :
    string -> (char * int) * (char * int) -> board_type -> int -> unit
  (** Given string word and and length 2 list with starting and ed , add the
      word to the board. *)

  val check_word_fit :
    board_type -> string -> (char * int) * (char * int) -> bool
  (** Given a board, and a word and its proposed location, checks whether it
      fits on the board. *)

  val init_letter_bank : char list -> letter_bank
  (** Returns a char list representing the letter bank of Scrabble (the letter
      bank is a multiset of English alphabet letters). If the input char list is
      [], then the official Scrabble letter bank is created. Otherwise, the
      letter bank contains exactly the char list which is inputted. Only called
      once per game, at the very beginning. *)

  val update_bank : letter_bank -> char list -> letter_bank
  (** Given a [letter_bank] and a list of sampled letters [sampled], returns a
      new letter bank without the sampled input. Returns unchanged letter_bank
      if sampled is empty list. *)

  val to_list_bank : letter_bank -> char list
  (** Given [bank] of type letter_bank, returns char list representation of the
      letter bank. *)
end

module ScrabbleBoard : BoardType
(** Scrabble Board *)
