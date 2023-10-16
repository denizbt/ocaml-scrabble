open Board

module type PlayerType = sig
  type t

  val create : char list -> t
  val current_tiles : t -> char list
  val score : t -> int
  val check_word : t -> string -> bool
  val make_play : t -> string -> char * int -> char * int -> bool
end

module SinglePlayer : PlayerType = struct
  type t = {
    score : int;
    tiles : char list;
  }
  (** Representation type of the model. *)

  let create (lst : char list) : t = { score = 0; tiles = lst }
  (*creates a player given a list of tile letters they have*)

  (** Returns their current tiles *)
  let current_tiles (player : t) : char list = player.tiles

  (** Returns their current score*)
  let score (player : t) : int = player.score

  let rec string_to_char_list (input : string) : char list =
    match input with
    | "" -> []
    | x ->
        String.get x 0
        :: string_to_char_list (String.sub x 1 (String.length x - 1))

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

  (*Checks if starting and ending is in valid direction (horizontal and
    vertical), if the word is in the dictionary, and if the tiles are in the
    user's hand (TEMP)*)
  let check_word (player : t) (input : string) : bool =
    let tiles_avail = player.tiles in
    let tiles_used = string_to_char_list input in
    contains_chars tiles_avail tiles_used

  (** Given word, starting position, and ending position, sends the information
      to the board to be played. *)
  let make_play (player : t) (input : string) (starter : char * int)
      (ending : char * int) : bool =
    failwith "unimplemented"
  (*if check_word player input then if Board.add_word input (starter, ending)
    then player.tiles = _ else false else false *)
end
