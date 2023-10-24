module type PlayerType = sig
  type t

  val create_player : char list -> int -> t
  val current_tiles : t -> char list
  val score : t -> int
  val print_tiles : t -> string
  val update_tiles : t -> char list -> t
  val update_score : t -> int -> t
end

module SinglePlayer : PlayerType = struct
  type t = {
    score : int;
    tiles : char list;
  }
  (** Representation type of the model. *)

  let create_player (lst : char list) (pt : int) : t =
    { score = pt; tiles = lst }
  (*creates a player given a list of tile letters they have*)

  (** Returns their current tiles *)
  let current_tiles (player : t) : char list = player.tiles

  (** Returns their current score*)
  let score (player : t) : int = player.score

  (* Given list of tiles (i.e. a player's hand), returns ASCII representation of
     the tiles. *)
  let rec ascii_string tiles : string =
    match tiles with
    | [] -> ""
    | h :: [] -> " | " ^ String.make 1 h
    | h :: t -> (" | " ^ String.make 1 h) ^ ascii_string t

  (** Returns string which is ASCII representation of given [player] hand. *)
  let print_tiles (player : t) : string = ascii_string (current_tiles player)

  (** Given a player and a list of new sampled tiles, returns a player with an
      updated list of tiles. *)
  let update_tiles (player : t) (sampled : char list) : t =
    create_player (sampled @ current_tiles player) (score player)

  (*Given an non-negative integer n, adds n to score and returns back the
    player*)
  let update_score (player : t) (n : int) : t =
    create_player player.tiles (player.score + n)
end

(* Given an inputted string and a string list dictionary, checks the dictionary
   if the input is in there*)
let rec search_dict (input : string) (dict_lst : string list) : bool =
  match dict_lst with
  | [] -> false
  | h :: t ->
      if String.get h 0 > String.get input 0 then false
      else if h = input then true
      else search_dict input t

(* Creates dictionary as a string list and returns whether the word is in the
   dictionary. Helper function used in check_word*)
let in_dictionary (input : string) : bool =
  let file = "src/scrabble_dictionary.txt" in
  let dict = file |> In_channel.open_bin |> In_channel.input_all in
  let dict_lst = String.split_on_char '\n' dict in
  search_dict (String.uppercase_ascii input ^ "\r") dict_lst

(*Converts a string to a char list. Helper function used in check_word (used to
  be in make_play too)*)
let rec string_to_char_list (input : string) : char list =
  match input with
  | "" -> []
  | x ->
      String.get x 0
      :: string_to_char_list (String.sub x 1 (String.length x - 1))

(*Checks a given char list if the second given char list has all of it's
  elements in the first. Helper function uesd by check_word *)
let rec contains_chars (avail : char list) (used : char list) : bool =
  match used with
  | [] -> true
  | h :: t ->
      if List.find_opt (fun x -> if x = h then true else false) avail = None
      then false
      else
        contains_chars
          (List.filter (fun x -> if x = h then false else true) avail)
          t

(*Verifies that the inputted starting position and ending position of an
  inputted word is possible / in vertical or horizontal direction. Helper
  function used in check_word*)
let rec valid_dir (starter : char * int) (ending : char * int) : bool =
  if fst starter = fst ending then true
  else if snd starter = snd ending then true
  else false

(*Checks if starting and ending is in valid direction (horizontal and vertical),
  if the word is in the dictionary, and if the tiles are in the user's hand
  (TEMP)*)
let check_word (player_hand : char list) (input : string) (starter : char * int)
    (ending : char * int) : bool =
  let tiles_used = string_to_char_list input in
  contains_chars player_hand tiles_used
  && valid_dir starter ending && in_dictionary input
