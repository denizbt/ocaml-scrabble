open Helper

module type PlayerType = sig
  type t

  val create_player : char list -> int -> bool -> t
  val current_tiles : t -> char list
  val score : t -> int
  val easy_mode : t -> bool
  val print_tiles : t -> string
  val check_tiles : t -> char list -> bool
  val update_tiles : t -> char list -> char list -> t
  val update_score : t -> int -> t
  val possible_words_from_tiles : t -> string list
end

module SinglePlayer : PlayerType = struct
  type t = {
    score : int;
    tiles : char list;
    easy_mode : bool;
  }
  (** Representation type of the model. *)

  let create_player (lst : char list) (pt : int) (mode : bool) : t =
    { score = pt; tiles = lst; easy_mode = mode }
  (*creates a player given a list of tile letters they have*)

  (** Returns their current tiles *)
  let current_tiles (player : t) : char list = player.tiles

  (** Returns their current score*)
  let score (player : t) : int = player.score

  let easy_mode (player : t) : bool = player.easy_mode

  (* Given list of tiles (i.e. a player's hand), returns ASCII representation of
     the tiles. *)
  let rec ascii_string tiles : string =
    match tiles with
    | [] -> ""
    | h :: [] -> " | " ^ String.make 1 h
    | h :: t -> (" | " ^ String.make 1 h) ^ ascii_string t

  (** Returns string which is ASCII representation of given [player] hand. *)
  let print_tiles (player : t) : string = ascii_string (current_tiles player)

  (* Helper function for update tiles which returns player's tiles without the
     letters used in the word. Requires that all of the chars in word are in
     prev_tiles. *)
  let rec remove_used_letters (prev_tiles : char list) (word : char list) :
      char list =
    match word with
    | [] -> prev_tiles
    | h :: t ->
        let x = List.find_opt (fun x -> x = h) prev_tiles in
        if x = None then failwith "Using letter which is not in player's list!"
        else
          remove_used_letters
            (Helper.list_without_elem prev_tiles (Option.get x))
            t

  (* Recursively pattern matches on needed_tiles *)
  let rec check_tiles (player : t) (needed_tiles : char list) : bool =
    match needed_tiles with
    | [] -> true
    | h :: t -> (
        match List.find_opt (fun x -> x = h) (current_tiles player) with
        | None -> false
        | Some v -> check_tiles player t)

  (* Returns a player with updated list of tiles given word *)
  let update_tiles (player : t) (used_letters : char list) (sampled : char list)
      : t =
    create_player
      (sampled @ remove_used_letters (current_tiles player) used_letters)
      (score player) player.easy_mode

  (* Given an non-negative integer n, adds n to score and returns back the
     player*)
  let update_score (player : t) (n : int) : t =
    create_player player.tiles (player.score + n) player.easy_mode

  (*Helper function that sees if list 2 only contains elements from list 1*)
  let rec has_all (lst1 : 'a list) (lst2 : 'a list) : bool =
    match lst2 with
    | [] -> true
    | h :: t ->
        if List.mem h lst1 then has_all (Helper.list_without_elem lst1 h) t
        else false

  (*Given a player, finds a list of possible words from their tiles*)
  let possible_words_from_tiles (player : t) : string list =
    let tiles = player.tiles in
    let dict_lst = Helper.create_dictionary in
    List.filter (fun x -> has_all tiles (Helper.char_list_of_string x)) dict_lst
end
