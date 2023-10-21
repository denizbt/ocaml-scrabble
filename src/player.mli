module type PlayerType = sig
  type t
  (** Representation type of the model. *)

  val create_player : char list -> int -> t
  (*creates a player given a list of tile letters and score that they have*)

  val current_tiles : t -> char list
  (** Returns a char list represnting the player's tiles/hands. *)

  val print_tiles : t -> string
  (** Returns string which is ASCII representation of given [player] hand. *)

  val update_tiles : t -> char list -> char list -> t
  (** Given a player and a list of new sampled tiles, returns a player with an
      updated list of tiles. *)

  val score : t -> int
  (** Returns their current score *)
end

module SinglePlayer : PlayerType

(*Checks if starting and ending is in valid direction (horizontal and vertical),
  if the word is in the dictionary, and if the tiles are in the user's hand
  (TEMP)*)
val check_word : char list -> string -> char * int -> char * int -> bool
