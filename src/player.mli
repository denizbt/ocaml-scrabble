module type PlayerType = sig
  type t
  (** Representation type of the model. *)

  val create_player : char list -> int -> t
  (** Creates a player given a list of tile letters they have. *)

  val current_tiles : t -> char list
  (** Returns the given player's current tiles *)

  val score : t -> int
  (** Returns their current score*)

  val print_tiles : t -> string
  (** Returns a string representation of the tiles in the player's hand. *)

  val update_tiles : t -> string -> char list -> t
  (** Given a list of new sampled tiles and the most recently played word,
      returns a new player with updated tiles. *)

  val update_score : t -> int -> t
  (** Given an non-negative integer n, adds n to score and returns back the
      player*)
end

module SinglePlayer : PlayerType

val check_word : char list -> string -> bool
(** Checks if the given [input] is in the scrabble dictionary, and if the tiles
    are in the player's hand. *)
