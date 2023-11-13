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

  val check_tiles : t -> char list -> bool
  (** Given a player and a list of tiles, returns true if they have all of the
      input tiles in their hand and false otherwise. *)

  val update_tiles : t -> char list -> char list -> t
  (** Given a list of new sampled tiles and a list of used letters, returns a
      new player with updated tiles. Requires that everything in used_letters is
      in player's current tiles. *)

  val update_score : t -> int -> t
  (** Given an non-negative integer n, adds n to score and returns back the
      player*)
end

module SinglePlayer : PlayerType
