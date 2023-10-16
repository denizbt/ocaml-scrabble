module type PlayerType = sig
  type t
  (** Representation type of the model. *)

  val create : char list -> t
  (*creates a player given a list of tile letters they have*)

  val current_tiles : t -> char list
  (** Returns their current tiles *)

  val score : t -> int
  (** Returns their current score*)

  val check_word : t -> string -> bool
  (** Check if given word is in the dictionary *)

  val make_play : t -> string -> char * int -> char * int -> bool
  (** Given word, starting position, and ending position, sends the information
      to the board to be played *)
end

module SinglePlayer : PlayerType
